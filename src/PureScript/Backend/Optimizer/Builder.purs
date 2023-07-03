module PureScript.Backend.Optimizer.Builder
  ( BuildEnv
  , BuildOptions
  , buildModules
  ) where

import Prelude

import Data.FoldableWithIndex (foldrWithIndex)
import Data.List (List, foldM)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.Analysis (BackendAnalysis)
import PureScript.Backend.Optimizer.Convert (BackendModule, OptimizationSteps, toBackendModule)
import PureScript.Backend.Optimizer.CoreFn (Ann, Ident, Module(..), Qualified)
import PureScript.Backend.Optimizer.Semantics (ExternImpl, InlineDirectiveMap)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval)

type BuildEnv =
  { implementations :: Map (Qualified Ident) (Tuple BackendAnalysis ExternImpl)
  , moduleCount :: Int
  , moduleIndex :: Int
  }

type BuildOptions m =
  { directives :: InlineDirectiveMap
  , foreignSemantics :: Map (Qualified Ident) ForeignEval
  , onPrepareModule :: BuildEnv -> Module Ann -> m (Module Ann)
  , onCodegenModule :: BuildEnv -> Module Ann -> BackendModule -> OptimizationSteps -> m Unit
  , traceIdents :: Set (Qualified Ident)
  }

-- | Builds modules given a _sorted_ list of modules.
-- | See `PureScript.Backend.Optimizer.CoreFn.Sort.sortModules`.
buildModules :: forall m. Monad m => BuildOptions m -> List (Module Ann) -> m Unit
buildModules options coreFnModules =
  void $ foldM go { directives: options.directives, implementations: Map.empty, moduleIndex: 0 } coreFnModules
  where
  moduleCount = List.length coreFnModules
  go { directives, implementations, moduleIndex } coreFnModule = do
    let buildEnv = { implementations, moduleCount, moduleIndex }
    coreFnModule'@(Module { name }) <- options.onPrepareModule buildEnv coreFnModule
    let
      Tuple optimizationSteps backendMod = toBackendModule coreFnModule'
        { currentModule: name
        , currentLevel: 0
        , toLevel: Map.empty
        , implementations
        , moduleImplementations: Map.empty
        , directives
        , dataTypes: Map.empty
        , foreignSemantics: options.foreignSemantics
        , rewriteLimit: 10_000
        , traceIdents: options.traceIdents
        , optimizationSteps: []
        }
      newImplementations =
        foldrWithIndex Map.insert implementations backendMod.implementations
    options.onCodegenModule (buildEnv { implementations = newImplementations }) coreFnModule' backendMod optimizationSteps
    pure
      { directives: foldrWithIndex Map.insert directives backendMod.directives
      , implementations: newImplementations
      , moduleIndex: moduleIndex + 1
      }
