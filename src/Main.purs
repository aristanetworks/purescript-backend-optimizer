module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Parallel (parTraverse)
import Control.Plus (empty)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.Foldable (oneOf)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
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
import PureScript.Backend.Codegen.EcmaScript (esCodegenModule, esForeignModulePath, esModulePath)
import PureScript.Backend.Convert (toBackendModule)
import PureScript.CoreFn (Ann, Import(..), Module(..), ModuleName(..))
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
                # ArgParser.default (Path.concat [ ".", "output-es" ])
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
                # ArgParser.default (Path.concat [ ".", "output-es" ])
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
    writeTextFile UTF8 (Path.concat [ outputDir, "package.json" ]) $ Json.stringify do
      Json.jsonSingletonObject "type" (Json.fromString "module")
    let
      go implementations = case _ of
        List.Nil -> pure unit
        List.Cons coreFnMod@(Module { name, foreign: foreignIdents, path }) mods -> do
          Console.log $ unwrap name
          let modPath = Path.concat [ outputDir, esModulePath name ]
          let backendMod = toBackendModule coreFnMod { currentModule: name, currentLevel: 0, toLevel: Map.empty, implementations, deps: Set.empty }
          let formatted = Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 180, ribbonRatio = 0.5 }) $ esCodegenModule backendMod
          writeTextFile UTF8 modPath formatted
          unless (Array.null foreignIdents) do
            let foreignFileName =  esForeignModulePath name
            let foreignOutputPath = Path.concat [ outputDir, foreignFileName ]
            let foreignSiblingPath = fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <> ".js"
            res <- attempt $ oneOf
              [ copyFile foreignSiblingPath foreignOutputPath
              , maybe empty (\dir -> copyFile (Path.concat [ dir, esModulePath name ]) foreignOutputPath) foreignDir
              , copyFile (Path.concat [ dirName, "foreign-es", esModulePath name ]) foreignOutputPath
              ]
            unless (isRight res) do
              Console.warn $ "  Foreign implementation missing"
          go (Map.union backendMod.implementations implementations) mods
    go Map.empty (sortCoreFnModules coreFnModules)
  Run { main: mainModule, bin, args, outputDir } -> do
    cwd <- liftEffect $ Path.resolve [] outputDir
    luaPath <- liftEffect $ Process.lookupEnv "LUA_PATH"
    let modulePath = esModulePath (ModuleName mainModule)
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
  case lmap Json.printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
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

sortCoreFnModules :: forall a. Map ModuleName (Module a) -> List (Module a)
sortCoreFnModules initialModules = go initialModIndex List.Nil (Right <<< getModuleName <$> List.fromFoldable initialModules)
  where
  initialModIndex =
    (\mod -> { module: mod, visited: false }) <$> initialModules

  go modIndex acc = case _ of
    List.Nil ->
      List.reverse acc
    List.Cons (Left mod) names ->
      go modIndex (List.Cons mod acc) names
    List.Cons (Right name) names ->
      case Map.lookup name modIndex of
        Just modState@{ module: Module mod } -> do
          if modState.visited then
            go modIndex acc names
          else do
            let importNames = (\(Import _ imp) -> Right imp) <$> mod.imports
            let modIndex' = Map.insert name (modState { visited = true }) modIndex
            go modIndex' acc (List.fromFoldable importNames <> List.Cons (Left modState.module) names)
        _ ->
          go modIndex acc names

getModuleName :: forall a. Module a -> ModuleName
getModuleName (Module mod) = mod.name