module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Backend (backendModule, codegenModule, luaModulePath, runBackendM)
import Control.Parallel (parTraverse, parTraverse_)
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Sync (mkdirRecursive)
import Node.Glob.Basic (expandGlobsCwd)
import Node.Path as Path
import Node.Process as Process
import PureScript.CoreFn (Ann, Module(..), ModuleName)
import PureScript.CoreFn.Json (decodeModule)

type Args =
  { globs :: Array String
  , outputDir :: String
  }

argParser :: ArgParser Args
argParser =
  ArgParser.fromRecord
    { globs:
        ArgParser.anyNotFlag "COREFN_GLOB"
          "Globs for corefn.json files."
          # ArgParser.unfolded1
    , outputDir:
        ArgParser.argument [ "--output-dir" ]
          "Output directory for backend files"
          # ArgParser.default (Path.concat [ ".", "output-lua" ])
    }
    <* ArgParser.flagHelp

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  let
    parsedArgs =
      ArgParser.parseArgs "purs-backend-project"
        "An example PureScript backend."
        argParser
        args
  case parsedArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right args' -> launchAff_ do
      compileModules args'

compileModules :: Args -> Aff Unit
compileModules { globs, outputDir } = do
  coreFnModules <-
    expandGlobsCwd globs >>=
      Array.fromFoldable
        >>> parTraverse readCoreFnModule
        >>> map (Array.catMaybes >>> Map.fromFoldable)
  liftEffect $ mkdirRecursive outputDir
  flip parTraverse_ coreFnModules \coreFnMod@(Module { name }) -> do
    let
      modPath = Path.concat [ outputDir, luaModulePath name ]
      backendMod = runBackendM
        { backendModules: Map.empty
        , coreFnModules: Map.empty
        , currentModule: name
        }
        0
        (codegenModule =<< backendModule coreFnMod)
      formatted =
        Dodo.print Dodo.plainText Dodo.twoSpaces backendMod
    writeTextFile UTF8 modPath formatted

readCoreFnModule :: String -> Aff (Maybe (Tuple ModuleName (Module Ann)))
readCoreFnModule filePath = do
  contents <- liftEffect <<< Buffer.toString UTF8 =<< FS.readFile filePath
  case lmap printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
    Left err -> do
      Console.error $ filePath <> ":\n" <> err
      pure Nothing
    Right mod@(Module { name }) ->
      pure $ Just (Tuple name mod)