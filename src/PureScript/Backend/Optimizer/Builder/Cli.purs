module PureScript.Backend.Optimizer.Builder.Cli where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Process as Process
import PureScript.Backend.Optimizer.Builder (BuildEnv, buildModules, coreFnModulesFromOutput)
import PureScript.Backend.Optimizer.Convert (BackendModule)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults as Defaults
import PureScript.Backend.Optimizer.Semantics (EvalRef, InlineDirective)
import PureScript.CST.Errors (printParseError)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module)

externalDirectivesFromFile :: FilePath -> Aff (Map EvalRef InlineDirective)
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
     , resolveExternalDirectives :: Aff (Map EvalRef InlineDirective)
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
        , onCodegenModule: options.onCodegenModule
        , onPrepareModule: options.onPrepareModule
        }
      options.onCodegenAfter
