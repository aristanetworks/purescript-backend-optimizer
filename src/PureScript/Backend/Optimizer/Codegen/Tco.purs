module PureScript.Backend.Optimizer.Codegen.Tco where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (fold, foldMap)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, ModuleName, Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendEffect(..), BackendSyntax(..), Level, Pair(..))

type LocalRef = Tuple (Maybe Ident) Level
type TcoScope = List TcoScopeItem
type TcoScopeItem = Tuple Ident (NonEmptyArray TcoRef)

data TcoRef
  = TcoTopLevel (Qualified Ident)
  | TcoLocal (Maybe Ident) Level

derive instance Eq TcoRef
derive instance Ord TcoRef

type TcoPop =
  { ident :: Ident
  , group :: NonEmptyArray TcoRef
  , index :: Int
  , stack :: List Ident
  }

popTcoScope :: TcoRef -> TcoScope -> Maybe TcoPop
popTcoScope ref = go List.Nil
  where
  go stack = case _ of
    List.Cons (Tuple ident group) rest
      | Just index <- NonEmptyArray.findIndex (eq ref) group ->
          Just { ident, group, index, stack }
      | otherwise ->
          go (List.Cons ident stack) rest
    _ ->
      Nothing

inTcoScope :: TcoRef -> TcoScope -> Boolean
inTcoScope ref = go
  where
  go = case _ of
    List.Cons (Tuple _ group) rest
      | NonEmptyArray.elem ref group ->
          true
      | otherwise ->
          go rest
    _ ->
      false

unwindTcoScope :: TcoScope -> Maybe (Tuple Ident (List Ident))
unwindTcoScope = go List.Nil
  where
  go pop = case _ of
    List.Cons (Tuple tcoIdent _) rest
      | List.null rest ->
          Just (Tuple tcoIdent pop)
      | otherwise ->
          go (List.Cons tcoIdent pop) rest
    List.Nil ->
      Nothing

type TcoEnv = Array (Tuple TcoRef Int)

newtype TcoUsage = TcoUsage
  { total :: Int
  , arities :: Set Int
  , call :: Int
  , readWrite :: Int
  }

instance Semigroup TcoUsage where
  append (TcoUsage a) (TcoUsage b) = TcoUsage
    { total: a.total + b.total
    , arities: Set.union a.arities b.arities
    , call: a.call + b.call
    , readWrite: a.readWrite + b.readWrite
    }

instance Monoid TcoUsage where
  mempty = TcoUsage
    { total: 0
    , arities: Set.empty
    , call: 0
    , readWrite: 0
    }

newtype TcoAnalysis = TcoAnalysis
  { usages :: Map TcoRef TcoUsage
  , tailCalls :: Map TcoRef Int
  , role :: TcoRole
  }

derive instance Newtype TcoAnalysis _

instance Semigroup TcoAnalysis where
  append (TcoAnalysis a) (TcoAnalysis b) = TcoAnalysis
    { usages: Map.unionWith append a.usages b.usages
    , tailCalls: Map.unionWith add a.tailCalls b.tailCalls
    , role: noTcoRole
    }

instance Monoid TcoAnalysis where
  mempty = TcoAnalysis
    { usages: Map.empty
    , tailCalls: Map.empty
    , role: noTcoRole
    }

type TcoRole =
  { joins :: Array TcoRef
  , isLoop :: Boolean
  }

usedTopLevel :: TcoAnalysis -> Set (Qualified Ident)
usedTopLevel (TcoAnalysis { usages }) = usages
  # Map.keys
  # Set.mapMaybe case _ of
      TcoTopLevel qual -> Just qual
      TcoLocal _ _ -> Nothing

noTcoRole :: TcoRole
noTcoRole = { joins: [], isLoop: false }

hasTcoRole :: TcoRole -> Boolean
hasTcoRole { joins, isLoop } = isLoop || not (Array.null joins)

data TcoExpr = TcoExpr TcoAnalysis (BackendSyntax TcoExpr)

unTcoExpr :: TcoExpr -> BackendSyntax TcoExpr
unTcoExpr (TcoExpr _ a) = a

tcoAnalysisOf :: TcoExpr -> TcoAnalysis
tcoAnalysisOf (TcoExpr a _) = a

overTcoAnalysis :: (TcoAnalysis -> TcoAnalysis) -> TcoExpr -> TcoExpr
overTcoAnalysis f (TcoExpr a b) = TcoExpr (f a) b

tcoCall :: TcoRef -> Int -> TcoAnalysis -> TcoAnalysis
tcoCall ident arity (TcoAnalysis s) = TcoAnalysis s
  { usages = Map.insertWith append ident (TcoUsage { total: 1, call: 1, arities: Set.singleton arity, readWrite: 0 }) s.usages
  , tailCalls = Map.insert ident 1 s.tailCalls
  }

tcoRefEffect :: TcoRef -> TcoAnalysis -> TcoAnalysis
tcoRefEffect ident (TcoAnalysis s) = TcoAnalysis s
  { usages = Map.insertWith append ident (TcoUsage { total: 1, call: 0, arities: Set.empty, readWrite: 1 }) s.usages
  }

tcoNoTailCalls :: TcoAnalysis -> TcoAnalysis
tcoNoTailCalls (TcoAnalysis s) = TcoAnalysis s
  { tailCalls = Map.empty
  , role = s.role { joins = [] }
  }

withTcoRole :: TcoRole -> TcoAnalysis -> TcoAnalysis
withTcoRole role (TcoAnalysis s) = TcoAnalysis s { role = role }

isUniformTailCall :: TcoAnalysis -> TcoRef -> Int -> Maybe Boolean
isUniformTailCall (TcoAnalysis s) ref arity = do
  numTailCalls <- Map.lookup ref s.tailCalls
  TcoUsage u <- Map.lookup ref s.usages
  case Set.toUnfoldable u.arities of
    [ n ] ->
      Just $ n == arity && u.total == numTailCalls
    _ ->
      Nothing

type TcoRefBinding =
  { ref :: TcoRef
  , analysis :: TcoAnalysis
  , arity :: Int
  }

tcoRefBinding :: TcoRef -> TcoExpr -> Maybe TcoRefBinding
tcoRefBinding ref (TcoExpr _ expr) = case expr of
  Abs args (TcoExpr analysis _) ->
    Just { ref, analysis, arity: NonEmptyArray.length args }
  UncurriedAbs args (TcoExpr analysis _) ->
    Just { ref, analysis, arity: Array.length args }
  _ ->
    Nothing

tcoRefBindings :: (Ident -> TcoRef) -> NonEmptyArray (Tuple Ident TcoExpr) -> Maybe (NonEmptyArray TcoRefBinding)
tcoRefBindings toTcoRef = traverse \(Tuple ident expr) -> tcoRefBinding (toTcoRef ident) expr

tcoEnvGroup :: (Ident -> TcoRef) -> NonEmptyArray (Tuple Ident NeutralExpr) -> TcoEnv
tcoEnvGroup toTcoRef = fold <<< traverse go <<< NonEmptyArray.toArray
  where
  go (Tuple ident (NeutralExpr expr)) =
    Tuple (toTcoRef ident) <$> syntacticArity expr

localTcoRefBindings :: Level -> NonEmptyArray (Tuple Ident TcoExpr) -> Maybe (NonEmptyArray TcoRefBinding)
localTcoRefBindings level = tcoRefBindings \ident -> TcoLocal (Just ident) level

localTcoEnvGroup :: Level -> NonEmptyArray (Tuple Ident NeutralExpr) -> TcoEnv
localTcoEnvGroup level = tcoEnvGroup \ident -> TcoLocal (Just ident) level

topLevelTcoRefBindings :: ModuleName -> NonEmptyArray (Tuple Ident TcoExpr) -> Maybe (NonEmptyArray TcoRefBinding)
topLevelTcoRefBindings mod = tcoRefBindings (TcoTopLevel <<< Qualified (Just mod))

topLevelTcoEnvGroup :: ModuleName -> NonEmptyArray (Tuple Ident NeutralExpr) -> TcoEnv
topLevelTcoEnvGroup mod = tcoEnvGroup (TcoTopLevel <<< Qualified (Just mod))

isTailCalledIn :: TcoAnalysis -> NonEmptyArray TcoRefBinding -> Boolean
isTailCalledIn analysis group = do
  let tailCalled = NonEmptyArray.mapMaybe (\b -> isUniformTailCall analysis b.ref b.arity) group
  not (Array.null tailCalled) && Array.all identity tailCalled

tcoRoleIsLoop :: NonEmptyArray TcoRefBinding -> Boolean
tcoRoleIsLoop group = NonEmptyArray.all (flip isTailCalledIn group <<< _.analysis) group

tcoRoleJoins :: TcoEnv -> TcoAnalysis -> NonEmptyArray TcoRefBinding -> Array TcoRef
tcoRoleJoins env analysis group = do
  guard (isTailCalledIn analysis group)
  Array.nub $ foldMap (\b -> Array.mapMaybe (\(Tuple ref arity) -> ref <$ (guard =<< isUniformTailCall b.analysis ref arity)) env) group

syntacticArity :: forall a. BackendSyntax a -> Maybe Int
syntacticArity = case _ of
  Abs args _ ->
    Just $ NonEmptyArray.length args
  UncurriedAbs args _ ->
    Just $ Array.length args
  _ ->
    Nothing

analyze :: TcoEnv -> NeutralExpr -> TcoExpr
analyze env (NeutralExpr expr) = case expr of
  Var ident ->
    TcoExpr (tcoCall (TcoTopLevel ident) 0 mempty) $ Var ident
  Local ident level ->
    TcoExpr (tcoCall (TcoLocal ident level) 0 mempty) $ Local ident level
  App hd@(NeutralExpr (Local ident level)) tl -> do
    let hd' = analyze env hd
    let tl' = overTcoAnalysis tcoNoTailCalls <<< analyze env <$> tl
    let analysis2 = tcoCall (TcoLocal ident level) (NonEmptyArray.length tl') (foldMap tcoAnalysisOf tl')
    TcoExpr analysis2 $ App hd' tl'
  App hd@(NeutralExpr (Var ident)) tl -> do
    let hd' = analyze env hd
    let tl' = overTcoAnalysis tcoNoTailCalls <<< analyze env <$> tl
    let analysis2 = tcoCall (TcoTopLevel ident) (NonEmptyArray.length tl') (foldMap tcoAnalysisOf tl')
    TcoExpr analysis2 $ App hd' tl'
  UncurriedApp hd@(NeutralExpr (Local ident level)) tl -> do
    let hd' = analyze env hd
    let tl' = overTcoAnalysis tcoNoTailCalls <<< analyze env <$> tl
    let analysis2 = tcoCall (TcoLocal ident level) (Array.length tl') (foldMap tcoAnalysisOf tl')
    TcoExpr analysis2 $ UncurriedApp hd' tl'
  UncurriedApp hd@(NeutralExpr (Var ident)) tl -> do
    let hd' = analyze env hd
    let tl' = overTcoAnalysis tcoNoTailCalls <<< analyze env <$> tl
    let analysis2 = tcoCall (TcoTopLevel ident) (Array.length tl') (foldMap tcoAnalysisOf tl')
    TcoExpr analysis2 $ UncurriedApp hd' tl'
  PrimEffect (EffectRefRead ref@(NeutralExpr (Local ident level))) -> do
    let ref' = analyze env ref
    let analysis = tcoRefEffect (TcoLocal ident level) mempty
    TcoExpr analysis $ PrimEffect (EffectRefRead ref')
  PrimEffect (EffectRefWrite ref@(NeutralExpr (Local ident level)) val) -> do
    let ref' = analyze env ref
    let val' = analyze env val
    let analysis = tcoRefEffect (TcoLocal ident level) $ tcoAnalysisOf val'
    TcoExpr analysis $ PrimEffect (EffectRefWrite ref' val')
  Branch branches def -> do
    let branches' = map (\(Pair a b) -> Pair (overTcoAnalysis tcoNoTailCalls (analyze env a)) (analyze env b)) branches
    let def' = analyze env <$> def
    let analysis = foldMap (foldMap tcoAnalysisOf) branches' <> foldMap tcoAnalysisOf def'
    TcoExpr analysis $ Branch branches' def'
  Let ident level binding body -> do
    let binding' = analyze env binding
    let body' = analyze env body
    let expr' = Let ident level binding' body'
    let refBinding = tcoRefBinding (TcoLocal ident level) binding'
    let role = { isLoop: false, joins: foldMap (tcoRoleJoins env (tcoAnalysisOf body') <<< pure) refBinding }
    case refBinding of
      Just rb | hasTcoRole role -> do
        let analysis = rb.analysis <> tcoAnalysisOf body'
        TcoExpr (withTcoRole role analysis) expr'
      _ -> do
        let analysis = tcoNoTailCalls (tcoAnalysisOf binding') <> tcoAnalysisOf body'
        TcoExpr analysis expr'
  LetRec level bindings body -> do
    let group = localTcoEnvGroup level bindings
    let env' = group <> env
    let bindings' = map (analyze env') <$> bindings
    let body' = analyze env' body
    let expr' = LetRec level bindings' body'
    let refBindings = localTcoRefBindings level bindings'
    let isLoop = maybe false tcoRoleIsLoop refBindings
    let role = { isLoop, joins: foldMap (tcoRoleJoins env (tcoAnalysisOf body')) refBindings }
    if hasTcoRole role then do
      let analysis = foldMap (foldMap _.analysis) refBindings <> tcoAnalysisOf body'
      TcoExpr (withTcoRole role analysis) expr'
    else do
      let analysis = tcoNoTailCalls (foldMap (foldMap tcoAnalysisOf) bindings') <> tcoAnalysisOf body'
      TcoExpr analysis expr'
  _ -> do
    let expr' = analyze env <$> expr
    TcoExpr (tcoNoTailCalls (foldMap tcoAnalysisOf expr')) expr'
