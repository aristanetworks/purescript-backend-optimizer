module PureScript.Backend.Optimizer.Codegen.EcmaScript.Builder where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Process as Process
import PureScript.Backend.Optimizer.Builder (BuildEnv, buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule, OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ann, Ident, Module, Qualified)
import PureScript.Backend.Optimizer.CoreFn.FromFile (coreFnModulesFromOutput)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults as Defaults
import PureScript.Backend.Optimizer.Semantics (InlineDirectiveMap)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval)
import PureScript.CST.Errors (printParseError)

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
        { directives: allDirectives
        , foreignSemantics: options.foreignSemantics
        , onCodegenModule: options.onCodegenModule
        , onPrepareModule: options.onPrepareModule
        , traceIdents: options.traceIdents
        }
      options.onCodegenAfter
