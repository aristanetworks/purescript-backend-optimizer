module PureScript.Backend.Optimizer.Codegen.EcmaScript.Builder where

import Prelude

import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Process as Process
import PureScript.Backend.Optimizer.Builder (BuildEnv, buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule)
import PureScript.Backend.Optimizer.CoreFn (Ann, Ident, Module, Qualified)
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (sortModules)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults as Defaults
import PureScript.Backend.Optimizer.Semantics (InlineDirectiveMap)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval)
import PureScript.CST.Errors (printParseError)

coreFnModulesFromOutput :: String -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (List (Module Ann)))
coreFnModulesFromOutput path = do
  { left, right } <- map separate $ expandGlobs path [ "**/corefn.json" ] >>= Array.fromFoldable >>> parTraverse readCoreFnModule
  case NonEmptyArray.fromArray left of
    Just errors ->
      pure $ Left errors
    Nothing ->
      pure $ Right $ sortModules right

readCoreFnModule :: String -> Aff (Either (Tuple FilePath String) (Module Ann))
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  case lmap Json.printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
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
     , foreignSemantics :: Map (Qualified Ident) ForeignEval
     , onCodegenBefore :: Aff Unit
     , onCodegenAfter :: Aff Unit
     , onCodegenModule :: BuildEnv -> Module Ann -> BackendModule -> Aff Unit
     , onPrepareModule :: BuildEnv -> Module Ann -> Aff (Module Ann)
     }
  -> Aff Unit
basicBuildMain options = do
  { coreFnDir, externalDirectives } <- sequential do
    { coreFnDir: _, externalDirectives: _ }
      <$> parallel options.resolveCoreFnDirectory
      <*> parallel options.resolveExternalDirectives
  let defaultDirectives = (parseDirectiveFile Defaults.defaultDirectives).directives
  let allDirectives = Map.union externalDirectives defaultDirectives
  coreFnModulesFromOutput coreFnDir >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      options.onCodegenBefore
      coreFnModules # buildModules
        { directives: allDirectives
        , foreignSemantics: options.foreignSemantics
        , onCodegenModule: options.onCodegenModule
        , onPrepareModule: options.onPrepareModule
        }
      options.onCodegenAfter
