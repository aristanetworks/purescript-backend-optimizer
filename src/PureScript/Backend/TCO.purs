module PureScript.Backend.TCO where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import PureScript.Backend.Semantics (NeutralExpr(..))
import PureScript.Backend.Syntax (BackendSyntax(..), Level, Pair(..))
import PureScript.CoreFn (Ident, Qualified)

newtype Call = Call
  { count :: Int
  , arities :: Set Int
  }

instance Semigroup Call where
  append (Call a) (Call b) = Call
    { count: a.count + b.count
    , arities: Set.union a.arities b.arities
    }

data TcoOptimization
  = TcoRecGroup
  | TcoNonRecTransitive

newtype TcoAnalysis = TcoAnalysis
  { calls :: Map (Tuple (Maybe Ident) Level) Call
  , tailCalls :: Map (Tuple (Maybe Ident) Level) Int
  , topLevelCalls :: Map (Qualified Ident) Call
  , topLevelTailCalls :: Map (Qualified Ident) Int
  , shouldOptimize :: Boolean
  }

derive instance Newtype TcoAnalysis _

instance Semigroup TcoAnalysis where
  append (TcoAnalysis a) (TcoAnalysis b) = TcoAnalysis
    { calls: Map.unionWith append a.calls b.calls
    , tailCalls: Map.unionWith add a.tailCalls b.tailCalls
    , topLevelCalls: Map.unionWith append a.topLevelCalls b.topLevelCalls
    , topLevelTailCalls: Map.unionWith add a.topLevelTailCalls b.topLevelTailCalls
    , shouldOptimize: false
    }

instance Monoid TcoAnalysis where
  mempty = TcoAnalysis
    { calls: Map.empty
    , tailCalls: Map.empty
    , topLevelCalls: Map.empty
    , topLevelTailCalls: Map.empty
    , shouldOptimize: false
    }

data TcoExpr = TcoExpr TcoAnalysis (BackendSyntax TcoExpr)

unTcoExpr :: TcoExpr -> BackendSyntax TcoExpr
unTcoExpr (TcoExpr _ a) = a

tcoAnalysisOf :: TcoExpr -> TcoAnalysis
tcoAnalysisOf (TcoExpr a _) = a

overTcoAnalysis :: (TcoAnalysis -> TcoAnalysis) -> TcoExpr -> TcoExpr
overTcoAnalysis f (TcoExpr a b) = TcoExpr (f a) b

tcoBound :: forall f. Foldable f => f (Tuple (Maybe Ident) Level) -> TcoAnalysis -> TcoAnalysis
tcoBound idents (TcoAnalysis s) = TcoAnalysis s
  { calls = foldr Map.delete s.calls idents
  , tailCalls = foldr Map.delete s.tailCalls idents
  }

tcoCall :: Tuple (Maybe Ident) Level -> Int -> TcoAnalysis -> TcoAnalysis
tcoCall ident arity (TcoAnalysis s) = TcoAnalysis s
  { calls = Map.insertWith append ident (Call { count: 1, arities: Set.singleton arity }) s.calls
  , tailCalls = Map.insert ident 1 s.tailCalls
  }

tcoTopLevelCall :: Qualified Ident -> Int -> TcoAnalysis -> TcoAnalysis
tcoTopLevelCall ident arity (TcoAnalysis s) = TcoAnalysis s
  { topLevelCalls = Map.insertWith append ident (Call { count: 1, arities: Set.singleton arity }) s.topLevelCalls
  , topLevelTailCalls = Map.insert ident 1 s.topLevelTailCalls
  }

tcoNoTailCalls :: TcoAnalysis -> TcoAnalysis
tcoNoTailCalls (TcoAnalysis s) = TcoAnalysis s
  { tailCalls = Map.empty
  , topLevelTailCalls = Map.empty
  }

tcoShouldOptimize :: TcoAnalysis -> TcoAnalysis
tcoShouldOptimize (TcoAnalysis s) = TcoAnalysis s { shouldOptimize = true }

isUniformTailCall :: Tuple (Maybe Ident) Level -> TcoAnalysis ->  Maybe Int
isUniformTailCall ref (TcoAnalysis s) = do
  numTailCalls <- Map.lookup ref s.tailCalls
  Call call <- Map.lookup ref s.calls
  case Set.toUnfoldable call.arities of
    [ n ] | n > 0 && call.count == numTailCalls -> Just n
    _ -> Nothing

syntacticArity :: forall a. BackendSyntax a -> Int
syntacticArity = case _ of
  Abs args _ -> NonEmptyArray.length args
  _ -> 0

analyze :: NeutralExpr -> TcoExpr
analyze (NeutralExpr expr) = case expr of
  Var ident ->
    TcoExpr (tcoTopLevelCall ident 0 mempty) $ Var ident
  Local ident level ->
    TcoExpr (tcoCall (Tuple ident level) 0 mempty) $ Local ident level
  Branch branches def -> do
    let branches' = map (\(Pair a b) -> Pair (overTcoAnalysis tcoNoTailCalls (analyze a)) (analyze b)) branches
    let def' = analyze <$> def
    TcoExpr (foldMap (foldMap tcoAnalysisOf) branches' <> foldMap tcoAnalysisOf def') $ Branch branches' def'
  LetRec level bindings body -> do
    let refs = flip Tuple level <<< Just <<< fst <$> bindings
    let bindings' = map analyze <$> bindings
    let body' = analyze body
    let analysis3 = foldMap (foldMap tcoAnalysisOf) bindings' <> tcoAnalysisOf body'
    -- case traverse (uniformTailCall analysis3) refs of
    --   Just arities
    --     | Array.all identity $ Array.zipWith (\a (Tuple _ (NeutralExpr b)) -> a == syntacticArity b) arities bindings ->
    --         TcoExpr (tcoShouldOptimize (tcoBound refs analysis3)) $ LetRec level bindings' body'
    --   _ ->
    TcoExpr (tcoBound refs analysis3) $ LetRec level bindings' body'
  Let ident level binding body -> do
    let binding' = analyze binding
    let body' = analyze body
    let analysis3 = tcoAnalysisOf body'
    let analysis4 = tcoAnalysisOf binding' <> tcoBound [ Tuple ident level ] analysis3
    let newExpr = Let ident level binding' body'
    case isUniformTailCall (Tuple ident level) analysis3  of
      Just n | n == syntacticArity (unwrap binding) ->
        TcoExpr analysis4  newExpr
      _ ->
        TcoExpr (tcoNoTailCalls analysis4) newExpr
  App hd@(NeutralExpr (Local ident level)) tl -> do
    let hd' = analyze hd
    let tl' = overTcoAnalysis tcoNoTailCalls <<< analyze <$> tl
    let analysis2 = tcoCall (Tuple ident level) (NonEmptyArray.length tl') (foldMap tcoAnalysisOf tl')
    TcoExpr analysis2 $ App hd' tl'
  App hd@(NeutralExpr (Var ident)) tl -> do
    let hd' = analyze hd
    let tl' = overTcoAnalysis tcoNoTailCalls <<< analyze <$> tl
    let analysis2 = tcoTopLevelCall ident (NonEmptyArray.length tl') (foldMap tcoAnalysisOf tl')
    TcoExpr analysis2 $ App hd' tl'
  App _ _ ->
    defaultAnalyze expr
  Abs _ _ ->
    defaultAnalyze expr
  Accessor _ _ ->
    defaultAnalyze expr
  Update _ _ ->
    defaultAnalyze expr
  CtorSaturated _ _ _ ->
    defaultAnalyze expr
  CtorDef _ _ ->
    defaultAnalyze expr
  EffectBind _ _ _ _ ->
    defaultAnalyze expr
  EffectPure _ ->
    defaultAnalyze expr
  Lit _ ->
    defaultAnalyze expr
  Test _ _ ->
    defaultAnalyze expr
  Fail _ ->
    defaultAnalyze expr

defaultAnalyze :: BackendSyntax NeutralExpr -> TcoExpr
defaultAnalyze expr = do
  let expr' = analyze <$> expr
  TcoExpr (tcoNoTailCalls (foldMap tcoAnalysisOf expr')) expr'