module Test.Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array (findMap)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Aff as Error
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobsCwd)
import Node.Path as Path
import Node.Process as Process
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.Codegen.EcmaScript (codegenModule)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Builder (coreFnModulesFromOutput)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Foreign (esForeignSemantics)
import PureScript.Backend.Optimizer.Convert (BackendModule)
import PureScript.Backend.Optimizer.CoreFn (Comment(..), Ident(..), Module(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import PureScript.Backend.Optimizer.Tracer.Printer (printModuleSteps)
import PureScript.CST.Lexer (lexToken)
import PureScript.CST.Types as CST
import Test.Utils (bufferToUTF8, copyFile, execWithStdin, loadModuleMain, mkdirp, spawnFromParent)

type TestArgs =
  { accept :: Boolean
  , filter :: NonEmptyArray String
  , traceIdents :: Set (Qualified Ident)
  }

data TraceChoice
  = TraceToOutput
  | TraceToStdOut

derive instance Eq TraceChoice

argParser :: ArgParser TestArgs
argParser =
  ArgParser.fromRecord
    { accept:
        ArgParser.flag [ "--accept", "-a" ]
          "Accepts snapshot output"
          # ArgParser.boolean
          # ArgParser.default false
    , filter:
        ArgParser.argument [ "--filter", "-f" ]
          "Filter tests matching a prefix"
          # ArgParser.unfolded1
          # ArgParser.default (pure "Snapshot.*")
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
    }

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case ArgParser.parseArgs "test" "" argParser cliArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right args ->
      launchAff_ $ runSnapshotTests args

runSnapshotTests :: TestArgs -> Aff Unit
runSnapshotTests { accept, filter, traceIdents } = do
  liftEffect $ Process.chdir $ Path.concat [ "backend-es", "test", "snapshots" ]
  spawnFromParent "spago" [ "build -u \"-g corefn\"" ]
  snapshotDir <- liftEffect Process.cwd
  snapshotPaths <- expandGlobsCwd [ "Snapshot.*.purs" ]
  outputRef <- liftEffect $ Ref.new Map.empty
  let snapshotsOut = Path.concat [ "..", "snapshots-out" ]
  let testOut = Path.concat [ "..", "test-out" ]
  mkdirp snapshotsOut
  mkdirp testOut
  coreFnModulesFromOutput "output" filter >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      let { directives } = parseDirectiveFile defaultDirectives
      copyFile (Path.concat [ "..", "..", "runtime.js" ]) (Path.concat [ testOut, "runtime.js" ])
      stepsRef <- liftEffect $ Ref.new []
      coreFnModules # buildModules
        { directives
        , foreignSemantics: Map.union coreForeignSemantics esForeignSemantics
        , onCodegenModule: \build (Module { name: ModuleName name, path }) backendMod optimizationSteps -> do
            let
              formatted =
                Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 180, ribbonRatio = 1.0 }) $
                  codegenModule { intTags: false } build.implementations backendMod
            let testFileDir = Path.concat [ testOut, name ]
            let testFilePath = Path.concat [ testFileDir, "index.js" ]
            mkdirp testFileDir
            FS.writeTextFile UTF8 testFilePath formatted
            unless (Set.isEmpty backendMod.foreign) do
              let foreignSiblingPath = fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <> ".js"
              let foreignOutputPath = Path.concat [ testFileDir, "foreign.js" ]
              copyFile foreignSiblingPath foreignOutputPath
            when (Set.member (Path.concat [ snapshotDir, path ]) snapshotPaths) do
              void $ liftEffect $ Ref.modify (Map.insert name (Tuple formatted (hasFails backendMod))) outputRef
            unless (Array.null optimizationSteps) do
              liftEffect $ Ref.modify_ (flip Array.snoc (Tuple backendMod.name optimizationSteps)) stepsRef
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        , traceIdents
        }
      allSteps <- liftEffect (Ref.read stepsRef)
      unless (Array.null allSteps) do
        let allDoc = Dodo.foldWithSeparator (Dodo.break <> Dodo.break) $ uncurry printModuleSteps <$> allSteps
        FS.writeTextFile UTF8 "optimization-traces.txt" $ Dodo.print Dodo.plainText Dodo.twoSpaces allDoc
      outputModules <- liftEffect $ Ref.read outputRef
      results <- forWithIndex outputModules \name (Tuple output failsWith) -> do
        let
          snapshotFilePath = Path.concat [ snapshotsOut, name <> ".js" ]
          runAcceptedTest = do
            result <- attempt $ foldMap liftEffect =<< loadModuleMain =<< liftEffect (Path.resolve [ testOut, name ] "index.js")
            case result of
              Left err | matchesFail err failsWith -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                Console.log $ Error.message err
                pure false
              Right _ | isJust failsWith -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " succeeded when it should have failed."
                pure false
              _ ->
                pure true
        attempt (FS.readTextFile UTF8 snapshotFilePath) >>= case _ of
          Left _ -> do
            Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
            FS.writeTextFile UTF8 snapshotFilePath output
            pure true
          Right prevOutput
            | output == prevOutput ->
                runAcceptedTest
            | accept -> do
                Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
                FS.writeTextFile UTF8 snapshotFilePath output
                runAcceptedTest
            | otherwise -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                diff <- bufferToUTF8 <<< _.stdout =<< execWithStdin ("diff " <> snapshotFilePath <> " -") output
                Console.log diff
                pure false
      unless (Foldable.and results) do
        liftEffect $ Process.exit 1

hasFails :: BackendModule -> Maybe String
hasFails = findMap go <<< _.comments
  where
  go = case _ of
    LineComment comm ->
      String.stripPrefix (Pattern "@fails ") (String.trim comm)
    _ ->
      Nothing

matchesFail :: Error -> Maybe String -> Boolean
matchesFail err = case _ of
  Just msg ->
    not $ String.contains (Pattern msg) $ Error.message err
  Nothing ->
    true
