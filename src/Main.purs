module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Backend (backendModule, codegenModule, luaForeignModulePath, luaModulePath, runBackendM)
import Control.Parallel (parTraverse, parTraverse_)
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.Foldable (oneOf)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.FS.Sync (mkdirRecursive)
import Node.Glob.Basic (expandGlobsCwd)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import PureScript.CoreFn (Ann, Module(..), ModuleName)
import PureScript.CoreFn.Json (decodeModule)

type Args =
  { globs :: Array String
  , outputDir :: String
  , foreignDir :: String
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
    , foreignDir:
        ArgParser.argument [ "--foreign-dir" ]
          "Directory for foreign module implementations"
          # ArgParser.default (Path.concat [ ".", "foreign-lua" ])
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
compileModules { globs, outputDir, foreignDir } = do
  coreFnModules <-
    expandGlobsCwd globs >>=
      Array.fromFoldable
        >>> parTraverse readCoreFnModule
        >>> map (Array.catMaybes >>> Map.fromFoldable)
  liftEffect $ mkdirRecursive outputDir
  flip parTraverse_ coreFnModules \coreFnMod@(Module { name, foreign: foreignIdents, path }) -> do
    let
      modPath = Path.concat [ outputDir, luaModulePath name <> ".lua" ]
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
    unless (Array.null foreignIdents) do
      let foreignFileName =  luaForeignModulePath name <> ".lua"
      res <- attempt $ oneOf
        [ copyFile (fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <> ".lua") (Path.concat [ outputDir, foreignFileName ])
        , copyFile (Path.concat [ foreignDir, foreignFileName ]) (Path.concat [ outputDir, foreignFileName ])
        ]
      unless (isRight res) do
        Console.warn $ "Foreign implementation missing for " <> (unwrap name)

readCoreFnModule :: FilePath -> Aff (Maybe (Tuple ModuleName (Module Ann)))
readCoreFnModule filePath = do
  contents <- liftEffect <<< Buffer.toString UTF8 =<< FS.readFile filePath
  case lmap printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
    Left err -> do
      Console.error $ filePath <> ":\n" <> err
      pure Nothing
    Right mod@(Module { name }) ->
      pure $ Just (Tuple name mod)

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = makeAff \k -> do
  src <- createReadStream from
  dst <- createWriteStream to
  res <- Stream.pipe src dst
  Stream.onError src (k <<< Left)
  Stream.onError dst (k <<< Left)
  Stream.onError res (k <<< Left)
  Stream.onFinish res (k (Right unit))
  pure $ effectCanceler do
    Stream.destroy res
    Stream.destroy dst
    Stream.destroy src