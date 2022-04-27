module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Backend2 (toBackendModule, luaCodegenModule, luaForeignModulePath, luaModulePath)
import Control.Parallel (parTraverse, parTraverse_)
import Control.Plus (empty)
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.Foldable (oneOf)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, error, launchAff_, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.FS.Sync (mkdirRecursive)
import Node.Glob.Basic (expandGlobsCwd)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import PureScript.CoreFn (Ann, Module(..), ModuleName(..))
import PureScript.CoreFn.Json (decodeModule)

data Args
  = Build
      { globs :: Array String
      , outputDir :: String
      , foreignDir :: Maybe String
      }
  | Run
      { main :: String
      , bin :: String
      , args :: Array String
      , outputDir :: String
      }

argParser :: ArgParser Args
argParser = ArgParser.choose "command"
  [ Build <$> ArgParser.command [ "build" ]
      "Build a project from corefn modules."
      do
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
                # ArgParser.optional
          }
          <* ArgParser.flagHelp
  , Run <$> ArgParser.command [ "run" ]
      "Run a module."
      do
        ArgParser.fromRecord
          { main:
              ArgParser.anyNotFlag "MODULE_NAME"
                "Main entry point module name."
          , bin:
              ArgParser.argument [ "--bin" ]
                "Path to lua bin"
                # ArgParser.default "lua"
          , args:
              ArgParser.rest
                "Command line arguments to script."
                # ArgParser.default []
          , outputDir:
              ArgParser.argument [ "--output-dir" ]
                "Output directory for backend files"
                # ArgParser.default (Path.concat [ ".", "output-lua" ])
          }
  ]

main :: FilePath -> Effect Unit
main dirName = do
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
      compileModules dirName args'

compileModules :: FilePath -> Args -> Aff Unit
compileModules dirName = case _ of
  Build { globs, outputDir, foreignDir } -> do
    coreFnModules <-
      expandGlobsCwd globs >>=
        Array.fromFoldable
          >>> parTraverse readCoreFnModule
          >>> map (Array.catMaybes >>> Map.fromFoldable)
    liftEffect $ mkdirRecursive outputDir
    flip parTraverse_ coreFnModules \coreFnMod@(Module { name, foreign: foreignIdents, path }) -> do
      let modPath = Path.concat [ outputDir, luaModulePath name <> ".lua" ]
      let backendMod = luaCodegenModule $ toBackendModule coreFnMod { currentModule: name, currentLevel: 0 }
      let formatted = Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 320, ribbonRatio = 0.5 }) backendMod
      writeTextFile UTF8 modPath formatted
      unless (Array.null foreignIdents) do
        let foreignFileName =  luaForeignModulePath name <> ".lua"
        let foreignOutputPath = Path.concat [ outputDir, foreignFileName ]
        let foreignSiblingPath = fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <> ".lua"
        Console.log $ Path.concat [ dirName, "foreign-lua", luaModulePath name <> ".lua" ]
        res <- attempt $ oneOf
          [ copyFile foreignSiblingPath foreignOutputPath
          , maybe empty (\dir -> copyFile (Path.concat [ dir, luaModulePath name <> ".lua" ]) foreignOutputPath) foreignDir
          , copyFile (Path.concat [ dirName, "foreign-lua", luaModulePath name <> ".lua" ]) foreignOutputPath
          ]
        unless (isRight res) do
          Console.warn $ "Foreign implementation missing for " <> unwrap name
  Run { main: mainModule, bin, args, outputDir } -> do
    cwd <- liftEffect $ Path.resolve [] outputDir
    luaPath <- liftEffect $ Process.lookupEnv "LUA_PATH"
    let modulePath = luaModulePath (ModuleName mainModule)
    let script = "require('runtime');require('" <> modulePath <> "').main()"
    buffer <- liftEffect $ ChildProcess.execSync
      (String.joinWith " " ([bin,  "-e", show script ] <> args))
      defaultExecSyncOptions
        { env = Just $ Object.fromFoldable
            [ Tuple "LUA_PATH" $ String.joinWith ";" $ Array.catMaybes
                [ Just $ Path.concat [ cwd, "?.lua" ]
                , Just $ Path.concat [ dirName, "runtime", "?.lua" ]
                , luaPath
                ]
            ]
        , cwd = Just cwd
        }
    makeAff \k -> Stream.write Process.stdout buffer (k (Right unit)) $> mempty

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
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
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
