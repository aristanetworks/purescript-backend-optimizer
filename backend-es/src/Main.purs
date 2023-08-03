module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Plus (empty)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..), isRight)
import Data.Foldable (foldMap, for_, oneOf)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard, power)
import Data.Newtype (unwrap)
import Data.Number.Format as Number
import Data.Posix.Signal (Signal(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, effectCanceler, error, launchAff_, makeAff, nonCanceler, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.ChildProcess (Exit(..), StdIOBehaviour(..), defaultSpawnOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import PureScript.Backend.Optimizer.Codegen.EcmaScript (codegenModule, esModulePath)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Builder (basicBuildMain, externalDirectivesFromFile)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Foreign (esForeignSemantics)
import PureScript.Backend.Optimizer.Convert (OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Module(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import PureScript.Backend.Optimizer.Tracer.Printer (printModuleSteps)
import PureScript.CST.Lexer (lexToken)
import PureScript.CST.Lexer as Lexer
import PureScript.CST.Types (Token(..))
import PureScript.CST.Types as CST
import Unsafe.Coerce (unsafeCoerce)
import Version as Version

type BuildArgs =
  { coreFnDir :: FilePath
  , outputDir :: FilePath
  , foreignDir :: Maybe FilePath
  , directivesFile :: Maybe FilePath
  , intTags :: Boolean
  , traceIdents :: Set (Qualified Ident)
  , printTiming :: Boolean
  }

buildArgsParser :: ArgParser BuildArgs
buildArgsParser =
  ArgParser.fromRecord
    { coreFnDir:
        ArgParser.argument [ "--corefn-dir" ]
          "Path to input directory containing corefn.json files.\n\
          \Defaults to './output'."
          # ArgParser.default (Path.concat [ ".", "output" ])
    , outputDir:
        ArgParser.argument [ "--output-dir" ]
          "Path to output directory for backend files.\n\
          \Defaults to './output-es'."
          # ArgParser.default (Path.concat [ ".", "output-es" ])
    , foreignDir:
        ArgParser.argument [ "--foreign-dir" ]
          "Path to directory for foreign module overrides (optional)."
          # ArgParser.optional
    , directivesFile:
        ArgParser.argument [ "--directives" ]
          "Path to file that defines external inline directives (optional)."
          # ArgParser.optional
    , intTags:
        ArgParser.flag [ "--int-tags" ]
          "Use integers for tags in codegen instead of strings."
          # ArgParser.boolean
          # ArgParser.default false
    , traceIdents:
        ArgParser.argument [ "--trace-rewrites" ]
          "Traces rewrite passes for a top-level definition during optimizations.\n\
          \Outputs results to optimization-traces.txt."
          # ArgParser.unformat "QUALIFIED_NAME"
              ( \str ->
                  case lexToken str of
                    Right (CST.TokLowerName (Just (CST.ModuleName mod)) ident) ->
                      Right $ Set.singleton $ Qualified (Just (ModuleName mod)) (Ident ident)
                    _ ->
                      Left $ "Unable to parse qualified name: " <> str
              )
          # ArgParser.folded
    , printTiming:
        ArgParser.flag [ "--timing" ]
          "Print timing data for modules."
          # ArgParser.boolean
          # ArgParser.default false
    }

data TargetPlatform = Browser | Node

type BundleArgs =
  { entryModule :: ModuleName
  , targetFile :: FilePath
  , targetPlatform :: TargetPlatform
  , noBuild :: Boolean
  , minify :: Boolean
  , sourceMaps :: Boolean
  }

bundleArgsParser :: ArgParser BundleArgs
bundleArgsParser =
  ArgParser.fromRecord
    { entryModule:
        ArgParser.argument [ "-m", "--main" ]
          "Module to be used as the entry point.\n\
          \Defaults to 'Main'."
          # ArgParser.unformat "MODULE_NAME" parseModuleName
          # ArgParser.default (ModuleName "Main")
    , targetFile:
        ArgParser.argument [ "-t", "--to" ]
          "The target file path.\n\
          \Defaults to './index.js'."
          # ArgParser.default "index.js"
    , targetPlatform:
        ArgParser.argument [ "-p", "--platform" ]
          "Bundle platform 'browser' or 'node'.\n\
          \Defaults to 'browser'."
          # ArgParser.unformat "PLATFORM" parsePlatform
          # ArgParser.default Browser
    , noBuild:
        ArgParser.flag [ "-s", "--no-build" ]
          "Skip the build step."
          # ArgParser.boolean
          # ArgParser.default false
    , minify:
        ArgParser.flag [ "-y", "--minify" ]
          "Minifies the bundle."
          # ArgParser.boolean
          # ArgParser.default false
    , sourceMaps:
        ArgParser.flag [ "-x", "--source-maps" ]
          "Generate source maps for the bundle."
          # ArgParser.boolean
          # ArgParser.default false
    }
  where
  parseModuleName :: String -> Either String ModuleName
  parseModuleName str = case Lexer.lexToken str of
    Right (TokUpperName mn name) ->
      Right $ ModuleName $ foldMap ((_ <> ".") <<< unwrap) mn <> name
    _ ->
      Left $ "Invalid module name: " <> str

  parsePlatform :: String -> Either String TargetPlatform
  parsePlatform = case _ of
    "browser" -> Right Browser
    "node" -> Right Node
    str -> Left $ "Invalid platform: " <> str

data Command
  = Build BuildArgs
  | BundleApp BundleArgs BuildArgs
  | BundleModule BundleArgs BuildArgs

esArgParser :: ArgParser Command
esArgParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "build" ]
        "Builds ES code from corefn.json files"
        do
          Build <$> buildArgsParser <* ArgParser.flagHelp
    , ArgParser.command [ "bundle-app" ]
        "Bundles the project into an executable"
        do
          (BundleApp <$> bundleArgsParser <*> buildArgsParser)
            <* ArgParser.flagHelp
    , ArgParser.command [ "bundle-module" ]
        "Bundles the project into a module"
        do
          (BundleModule <$> bundleArgsParser <*> buildArgsParser)
            <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show the current version of purs-backend-es." Version.version

parseArgs :: Effect (Either ArgParser.ArgError Command)
parseArgs = do
  cliArgs <- Array.drop 2 <$> Process.argv
  pure $ ArgParser.parseArgs "purs-backend-es"
    "A PureScript backend for modern ECMAScript."
    esArgParser
    cliArgs

type BuildState =
  { currentStartTime :: Ref (Maybe Instant)
  , startTime :: Instant
  , steps :: Ref (Array (Tuple ModuleName OptimizationSteps))
  }

main :: FilePath -> Effect Unit
main cliRoot =
  parseArgs >>= case _ of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right (Build args) -> launchAff_ do
      buildCmd args
    Right (BundleModule bundleArgs args) -> launchAff_ do
      unless bundleArgs.noBuild $ buildCmd args
      bundleCmd false bundleArgs args
    Right (BundleApp bundleArgs args) -> launchAff_ do
      unless bundleArgs.noBuild $ buildCmd args
      bundleCmd true bundleArgs args
  where
  makeBuildState :: Effect BuildState
  makeBuildState = do
    startTime <- now
    currentStartTime <- Ref.new Nothing
    steps <- Ref.new []
    pure { currentStartTime, startTime, steps }

  buildCmd :: BuildArgs -> Aff Unit
  buildCmd args = liftEffect makeBuildState >>= \state -> basicBuildMain
    { resolveCoreFnDirectory: pure args.coreFnDir
    , resolveExternalDirectives: map (fromMaybe Map.empty) $ traverse externalDirectivesFromFile args.directivesFile
    , foreignSemantics: Map.union coreForeignSemantics esForeignSemantics
    , onCodegenBefore: do
        mkdirp args.outputDir
        writeTextFile UTF8 (Path.concat [ args.outputDir, "package.json" ]) esModulePackageJson
        copyFile (Path.concat [ cliRoot, "runtime.js" ]) (Path.concat [ args.outputDir, "runtime.js" ])
    , onCodegenAfter: do
        allSteps <- liftEffect (Ref.read state.steps)
        when args.printTiming do
          endTime <- liftEffect now
          let Milliseconds startMillis = Instant.unInstant state.startTime
          let Milliseconds endMillis = Instant.unInstant endTime
          Console.log $ "Total build time: " <> Number.toString (endMillis - startMillis) <> "ms"
        unless (Array.null allSteps) do
          let allDoc = Dodo.foldWithSeparator (Dodo.break <> Dodo.break) $ uncurry printModuleSteps <$> allSteps
          FS.writeTextFile UTF8 "optimization-traces.txt" $ Dodo.print Dodo.plainText Dodo.twoSpaces allDoc
    , onCodegenModule: \build (Module coreFnMod) backendMod@{ name: ModuleName name } optimizationSteps -> do
        let formatted = Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 180, ribbonRatio = 1.0 }) $ codegenModule { intTags: args.intTags } build.implementations backendMod
        let modPath = Path.concat [ args.outputDir, name ]
        mkdirp modPath
        writeTextFile UTF8 (Path.concat [ modPath, "index.js" ]) formatted
        unless (Set.isEmpty backendMod.foreign) do
          let foreignOutputPath = Path.concat [ modPath, "foreign.js" ]
          origPath <- liftEffect $ Path.resolve [ args.outputDir, ".." ] coreFnMod.path
          let foreignSiblingPath = fromMaybe origPath (String.stripSuffix (Pattern (Path.extname origPath)) origPath) <> ".js"
          res <- attempt $ oneOf
            [ maybe empty (\dir -> copyFile (Path.concat [ dir, esModulePath backendMod.name ]) foreignOutputPath) args.foreignDir
            , copyFile foreignSiblingPath foreignOutputPath
            ]
          unless (isRight res) do
            Console.log $ "  Foreign implementation missing."
        unless (Array.null optimizationSteps) do
          liftEffect $ Ref.modify_ (flip Array.snoc (Tuple backendMod.name optimizationSteps)) state.steps
        mbStartTime <- liftEffect $ Ref.read state.currentStartTime
        for_ mbStartTime \startTime -> do
          endTime <- liftEffect now
          let Milliseconds startMillis = Instant.unInstant startTime
          let Milliseconds endMillis = Instant.unInstant endTime
          writeString (Number.toString (endMillis - startMillis) <> "ms\n") Process.stdout

    , onPrepareModule: \build coreFnMod@(Module { name }) -> do
        let total = show build.moduleCount
        let index = show (build.moduleIndex + 1)
        let padding = power " " (SCU.length total - SCU.length index)
        let msg = "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
        if args.printTiming then do
          liftEffect $ flip Ref.write state.currentStartTime <<< Just =<< now
          writeString (msg <> "... ") Process.stdout
        else
          Console.log msg
        pure coreFnMod
    , traceIdents: args.traceIdents
    }

  bundleCmd :: Boolean -> BundleArgs -> BuildArgs -> Aff Unit
  bundleCmd shouldInvokeMain bundleArgs args = do
    entryPath <- liftEffect $ Path.resolve [] $ Path.concat
      [ args.outputDir
      , unwrap bundleArgs.entryModule
      , "index.js"
      ]
    let
      esBuildArgs = Array.filter (not <<< String.null)
        [ case bundleArgs.targetPlatform of
            Browser -> "--platform=browser"
            Node -> "--platform=node"
        , case bundleArgs.targetPlatform of
            Browser | shouldInvokeMain -> "--format=iife"
            _ -> "--format=esm"
        , guard bundleArgs.minify "--minify"
        , guard bundleArgs.sourceMaps "--sourcemap"
        , "--outfile=" <> bundleArgs.targetFile
        , "--bundle"
        ]
    if shouldInvokeMain then do
      spawnFromParentWithStdin "esbuild" esBuildArgs $ Just $ "import { main } from '" <> entryPath <> "'; main();"
    else
      spawnFromParentWithStdin "esbuild" (Array.snoc esBuildArgs entryPath) Nothing

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

writeString :: forall r. String -> Stream.Writable r -> Aff Unit
writeString str stream = makeAff \k -> do
  _ <- Stream.writeString stream UTF8 str (k <<< maybe (Right unit) Left)
  pure nonCanceler

mkdirp :: FilePath -> Aff Unit
mkdirp = flip FS.mkdir' { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }

esModulePackageJson :: String
esModulePackageJson = """{"type": "module"}"""

spawnFromParentWithStdin :: String -> Array String -> Maybe String -> Aff Unit
spawnFromParentWithStdin command args input = makeAff \k -> do
  childProc <- ChildProcess.spawn command args defaultSpawnOptions
    { stdio =
        [ Just $ Pipe
        , Just $ ShareStream (unsafeCoerce Process.stdout)
        , Just $ ShareStream (unsafeCoerce Process.stderr)
        ]
    }
  for_ input \inp -> do
    _ <- Stream.writeString (ChildProcess.stdin childProc) UTF8 inp mempty
    Stream.end (ChildProcess.stdin childProc) mempty
  ChildProcess.onExit childProc case _ of
    Normally code
      | code > 0 -> Process.exit code
      | otherwise -> k (Right unit)
    BySignal _ ->
      Process.exit 1
  pure $ effectCanceler do
    ChildProcess.kill SIGABRT childProc
