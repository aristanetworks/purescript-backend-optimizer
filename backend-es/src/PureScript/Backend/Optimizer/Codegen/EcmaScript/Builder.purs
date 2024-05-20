module PureScript.Backend.Optimizer.Codegen.EcmaScript.Builder where

import Prelude

import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Parallel (parTraverse)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Lazy as Lazy
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import JSON as JSON
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Process as Process
import PureScript.Backend.Optimizer.Analysis (BackendAnalysis)
import PureScript.Backend.Optimizer.Builder (BuildEnv, buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule, OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ann, Ident, Module, ModuleName(..), Qualified)
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule, printJsonDecodeError)
import PureScript.Backend.Optimizer.CoreFn.Sort (emptyPull, pullResult, resumePull, sortModules)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults as Defaults
import PureScript.Backend.Optimizer.Semantics (BackendExpr, Ctx, InlineDirectiveMap)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax)
import PureScript.CST.Errors (printParseError)

coreFnModulesFromOutput :: String -> NonEmptyArray String -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (List (Module Ann)))
coreFnModulesFromOutput path globs = runExceptT do
  paths <- Set.toUnfoldable <$> lift (expandGlobs path ((_ <> "/corefn.json") <$> NonEmptyArray.toArray globs))
  case NonEmptyArray.toArray globs of
    [ "**" ] ->
      sortModules <$> modulesFromPaths paths
    _ ->
      go <<< foldl resumePull emptyPull =<< modulesFromPaths paths
  where
  modulesFromPaths paths = ExceptT do
    { left, right } <- separate <$> parTraverse readCoreFnModule paths
    pure $ maybe (Right right) Left $ NonEmptyArray.fromArray left

  pathFromModuleName (ModuleName mn) =
    path <> "/" <> mn <> "/corefn.json"

  go pull = case pullResult pull of
    Left needed ->
      go <<< foldl resumePull pull =<< modulesFromPaths (pathFromModuleName <$> NonEmptySet.toUnfoldable needed)
    Right modules ->
      pure $ Lazy.force modules

readCoreFnModule :: String -> Aff (Either (Tuple FilePath String) (Module Ann))
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  case lmap printJsonDecodeError <<< decodeModule =<< JSON.parse contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod ->
      pure $ Right mod

externalDirectivesFromFile :: FilePath -> Aff InlineDirectiveMap
externalDirectivesFromFile filePath = do
  fileContent <- FS.readTextFile UTF8 filePath
  let { errors, directives } = parseDirectiveFile fileContent
  for_ errors \(Tuple directive { position, error }) -> do
    Console.warn $ "Invalid directive [" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "]"
    Console.warn $ "  " <> directive
    Console.warn $ "  " <> printParseError error
  pure directives

basicBuildMain
  :: { resolveCoreFnDirectory :: Aff FilePath
     , resolveExternalDirectives :: Aff InlineDirectiveMap
     , analyzeCustom :: Ctx -> BackendSyntax BackendExpr -> Maybe BackendAnalysis
     , foreignSemantics :: Map (Qualified Ident) ForeignEval
     , onCodegenBefore :: Aff Unit
     , onCodegenAfter :: Aff Unit
     , onCodegenModule :: BuildEnv -> Module Ann -> BackendModule -> OptimizationSteps -> Aff Unit
     , onPrepareModule :: BuildEnv -> Module Ann -> Aff (Module Ann)
     , traceIdents :: Set (Qualified Ident)
     }
  -> Aff Unit
basicBuildMain options = do
  { coreFnDir, externalDirectives } <- sequential do
    { coreFnDir: _, externalDirectives: _ }
      <$> parallel options.resolveCoreFnDirectory
      <*> parallel options.resolveExternalDirectives
  let defaultDirectives = (parseDirectiveFile Defaults.defaultDirectives).directives
  let allDirectives = Map.union externalDirectives defaultDirectives
  coreFnModulesFromOutput coreFnDir (pure "**") >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      options.onCodegenBefore
      coreFnModules # buildModules
        { analyzeCustom: options.analyzeCustom
        , directives: allDirectives
        , foreignSemantics: options.foreignSemantics
        , onCodegenModule: options.onCodegenModule
        , onPrepareModule: options.onPrepareModule
        , traceIdents: options.traceIdents
        }
      options.onCodegenAfter
