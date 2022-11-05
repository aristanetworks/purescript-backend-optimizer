module Test.Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
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
import PureScript.Backend.Optimizer.CoreFn (Module(..), ModuleName(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import Test.Utils (bufferToUTF8, execWithStdin, spawnFromParent)

type TestArgs =
  { accept :: Boolean
  , filter :: Array String
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
          # ArgParser.unfolded
          # map (map String.toLower)
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
  coreFnModulesFromOutput "output" >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      let
        { directives } = parseDirectiveFile defaultDirectives
        shouldCompare
          | Array.null filter = const true
          | otherwise = \name -> Array.any (isJust <<< flip String.stripPrefix (String.toLower name) <<< Pattern) filter
      coreFnModules # buildModules
        { directives
        , foreignSemantics: Map.union coreForeignSemantics esForeignSemantics
        , onCodegenModule: \build (Module { name: ModuleName name, path }) backend ->
            if Set.member (Path.concat [ snapshotDir, path ]) snapshotPaths && shouldCompare name then do
              let formatted = Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 180, ribbonRatio = 1.0 }) $ codegenModule { intTags: false } build.implementations backend
              void $ liftEffect $ Ref.modify (Map.insert name formatted) outputRef
            else
              mempty
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        }
      outputModules <- liftEffect $ Ref.read outputRef
      results <- forWithIndex outputModules \name output -> do
        let snapshotFilePath = Path.concat [ "..", "snapshots-out", name <> ".js" ]
        attempt (FS.readTextFile UTF8 snapshotFilePath) >>= case _ of
          Left _ -> do
            Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
            FS.writeTextFile UTF8 snapshotFilePath output
            pure true
          Right prevOutput
            | output == prevOutput -> do
                pure true
            | accept -> do
                Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
                FS.writeTextFile UTF8 snapshotFilePath output
                pure true
            | otherwise -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                diff <- bufferToUTF8 <<< _.stdout =<< execWithStdin ("diff " <> snapshotFilePath <> " -") output
                Console.log diff
                pure false
      unless (Foldable.and results) do
        liftEffect $ Process.exit 1
