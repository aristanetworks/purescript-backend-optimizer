module Test.Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Foldable as Foldable
import Data.Lazy as Lazy
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String.CodeUnits as SCU
import Data.String.CodeUnits as String
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
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
import PureScript.Backend.Optimizer.CoreFn (Bind(..), Binding(..), Ident(..), Module(..), ModuleName(..), importName, moduleName)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import Test.Utils (bufferToUTF8, copyFile, execWithStdin, loadModuleMain, mkdirp, spawnFromParent)

type TestArgs =
  { accept :: Boolean
  , filter :: NonEmptyArray String
  }

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
runSnapshotTests { accept, filter } = do
  liftEffect $ Process.chdir $ Path.concat [ "backend-es", "test", "snapshots" ]
  spawnFromParent "spago" [ "build", "-u", "-g corefn" ]
  snapshotDir <- liftEffect Process.cwd
  snapshotPaths <- expandGlobsCwd [ "*.purs" ]
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
      let depIndex = Map.fromFoldable $ moduleWithDependencies <$> coreFnModules
      let unitTestRoots = Set.fromFoldable $ List.mapMaybe (\m -> guard (hasMain m) $> moduleName m) coreFnModules
      let unitTestNeeds = transitiveDependencies depIndex unitTestRoots
      unless (Set.isEmpty unitTestNeeds) do
        copyFile (Path.concat [ "..", "..", "runtime.js" ]) (Path.concat [ testOut, "runtime.js" ])
      coreFnModules # buildModules
        { directives
        , foreignSemantics: Map.union coreForeignSemantics esForeignSemantics
        , onCodegenModule: \build (Module { name: ModuleName name, path }) backend -> do
            let
              formatted = Lazy.defer \_ ->
                Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 180, ribbonRatio = 1.0 }) $
                  codegenModule { intTags: false } build.implementations backend
            when (Set.member (Path.concat [ snapshotDir, path ]) snapshotPaths) do
              void $ liftEffect $ Ref.modify (Map.insert name formatted) outputRef
            when (Set.member (ModuleName name) unitTestNeeds) do
              let testFileDir = Path.concat [ testOut, name ]
              let testFilePath = Path.concat [ testFileDir, "index.js" ]
              mkdirp testFileDir
              FS.writeTextFile UTF8 testFilePath $ Lazy.force formatted
              unless (Set.isEmpty backend.foreign) do
                let foreignSiblingPath = fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <> ".js"
                let foreignOutputPath = Path.concat [ testFileDir, "foreign.js" ]
                copyFile foreignSiblingPath foreignOutputPath
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        }
      outputModules <- liftEffect $ Ref.read outputRef
      results <- forWithIndex outputModules \name output -> do
        let
          snapshotFilePath = Path.concat [ snapshotsOut, name <> ".js" ]
          runAcceptedTest =
            if Set.member (ModuleName name) unitTestRoots then do
              result <- attempt <<< liftEffect =<< loadModuleMain =<< liftEffect (Path.resolve [ testOut, name ] "index.js")
              case result of
                Left err -> do
                  Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                  Console.log $ Error.message err
                  pure false
                _ ->
                  pure true
            else
              pure true
        attempt (FS.readTextFile UTF8 snapshotFilePath) >>= case _ of
          Left _ -> do
            Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
            FS.writeTextFile UTF8 snapshotFilePath (Lazy.force output)
            pure true
          Right prevOutput
            | Lazy.force output == prevOutput ->
                runAcceptedTest
            | accept -> do
                Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
                FS.writeTextFile UTF8 snapshotFilePath $ Lazy.force output
                runAcceptedTest
            | otherwise -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                diff <- bufferToUTF8 <<< _.stdout =<< execWithStdin ("diff " <> snapshotFilePath <> " -") (Lazy.force output)
                Console.log diff
                pure false
      unless (Foldable.and results) do
        liftEffect $ Process.exit 1

hasMain :: forall a. Module a -> Boolean
hasMain (Module { decls, name: ModuleName name }) =
  isJust (String.stripPrefix (Pattern "Snapshot.") name) && Array.any go decls
  where
  go = case _ of
    NonRec (Binding _ (Ident "main") _) ->
      true
    _ ->
      false

moduleWithDependencies :: forall a. Module a -> Tuple ModuleName (Set ModuleName)
moduleWithDependencies (Module { imports, name }) = Tuple name (Set.fromFoldable $ Array.filter (_ /= name) $ importName <$> imports)

transitiveDependencies :: Map ModuleName (Set ModuleName) -> Set ModuleName -> Set ModuleName
transitiveDependencies index = foldl go mempty
  where
  go res name
    | Set.member name res =
        res
    | otherwise =
        case Map.lookup name index of
          Just deps ->
            foldl go (Set.insert name res) deps
          Nothing  ->
            res
