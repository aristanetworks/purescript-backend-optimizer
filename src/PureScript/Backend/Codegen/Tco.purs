module PureScript.Backend.Codegen.Tco where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid as Monoid
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import PureScript.Backend.Analysis (Usage(..))
import PureScript.Backend.Semantics (NeutralExpr(..))
import PureScript.Backend.Syntax (BackendSyntax(..), Level, Pair(..))
import PureScript.CoreFn (Ident, ModuleName, Qualified(..))

type LocalRef = Tuple (Maybe Ident) Level
type TcoScope = List TcoScopeItem
type TcoScopeItem = Tuple Ident (Set TcoRef)

data TcoRef
  = TcoTopLevel (Qualified Ident)
  | TcoLocal (Maybe Ident) Level

derive instance Eq TcoRef
derive instance Ord TcoRef

popTcoScope :: TcoRef -> TcoScope -> Maybe (Tuple Ident (List Ident))
popTcoScope ref = go List.Nil
  where
  go pop = case _ of
    List.Cons (Tuple tcoIdent tcoRefs) rest
      | Set.member ref tcoRefs ->
          Just (Tuple tcoIdent pop)
      | otherwise ->
        go (List.Cons tcoIdent pop) rest
    _ ->
      Nothing

inTcoScope :: TcoRef -> TcoScope -> Boolean
inTcoScope ref = go
  where
  go = case _ of
    List.Cons (Tuple _ tcoRefs) rest
      | Set.member ref tcoRefs ->
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

type TcoBinding =
  { arguments :: NonEmptyArray (Tuple (Maybe Ident) Level)
  , body :: TcoExpr
  , name :: Ident
  }

toTcoBinding :: Array (Tuple Ident TcoExpr) -> Maybe TcoBinding
toTcoBinding bindings = case bindings of
  [ Tuple ident (TcoExpr _ (Abs arguments body)) ] ->
    Just { arguments, body, name: ident }
  _ ->
    Nothing

type TcoEnv = Array (Tuple TcoRef Int)

newtype TcoAnalysis = TcoAnalysis
  { usages :: Map TcoRef Usage
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
  { usages = Map.insertWith append ident (Usage { count: 1, captured: false, arities: Set.singleton arity }) s.usages
  , tailCalls = Map.insert ident 1 s.tailCalls
  }

tcoNoTailCalls :: TcoAnalysis -> TcoAnalysis
tcoNoTailCalls (TcoAnalysis s) = TcoAnalysis s
  { tailCalls = Map.empty
  , role = noTcoRole
  }

withTcoRole :: TcoRole -> TcoAnalysis -> TcoAnalysis
withTcoRole role (TcoAnalysis s) = TcoAnalysis s { role = role }

isUniformTailCall :: TcoAnalysis -> TcoRef -> Int -> Maybe Boolean
isUniformTailCall (TcoAnalysis s) ref arity = do
  numTailCalls <- Map.lookup ref s.tailCalls
  Usage u <- Map.lookup ref s.usages
  case Set.toUnfoldable u.arities of
    [ n ] ->
      Just $ n > 0 && n == arity && u.count == numTailCalls
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
  _ ->
    Nothing

tcoRefBindings :: (Ident -> TcoRef) -> Array (Tuple Ident TcoExpr) -> Maybe (Array TcoRefBinding)
tcoRefBindings toTcoRef = traverse \(Tuple ident expr) -> tcoRefBinding (toTcoRef ident) expr

tcoEnvGroup :: (Ident -> TcoRef) -> Array (Tuple Ident NeutralExpr) -> TcoEnv
tcoEnvGroup toTcoRef bindings = do
  let env = bimap toTcoRef (syntacticArity <<< unwrap) <$> bindings
  Monoid.guard (Array.all (not <<< eq 0 <<< snd) env) env

localTcoRefBindings :: Level -> Array (Tuple Ident TcoExpr) -> Maybe (Array TcoRefBinding)
localTcoRefBindings level = tcoRefBindings \ident -> TcoLocal (Just ident) level

localTcoEnvGroup :: Level -> Array (Tuple Ident NeutralExpr) -> TcoEnv
localTcoEnvGroup level = tcoEnvGroup \ident -> TcoLocal (Just ident) level

topLevelTcoRefBindings :: ModuleName -> Array (Tuple Ident TcoExpr) -> Maybe (Array TcoRefBinding)
topLevelTcoRefBindings mod = tcoRefBindings (TcoTopLevel <<< Qualified (Just mod))

topLevelTcoEnvGroup :: ModuleName -> Array (Tuple Ident NeutralExpr) -> TcoEnv
topLevelTcoEnvGroup mod = tcoEnvGroup (TcoTopLevel <<< Qualified (Just mod))

isTailCalledIn :: TcoAnalysis -> Array TcoRefBinding -> Boolean
isTailCalledIn analysis group = do
  let tailCalled = Array.mapMaybe (\b -> isUniformTailCall analysis b.ref b.arity) group
  not (Array.null tailCalled) && Array.all identity tailCalled

tcoRoleIsLoop :: Array TcoRefBinding -> Boolean
tcoRoleIsLoop group = 
  Array.all (isLoop <<< _.analysis) group
  where
  isLoop :: TcoAnalysis -> Boolean
  isLoop = flip isTailCalledIn group

tcoRoleJoins :: TcoEnv -> TcoAnalysis -> Array TcoRefBinding -> Array TcoRef
tcoRoleJoins env analysis group = do
  guard (isTailCalledIn analysis group)
  Array.nub $ foldMap (\b -> Array.mapMaybe (\(Tuple ref arity) -> ref <$ (guard =<< isUniformTailCall b.analysis ref arity)) env) group

syntacticArity :: forall a. BackendSyntax a -> Int
syntacticArity = case _ of
  Abs args _ -> NonEmptyArray.length args
  _ -> 0

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
        let analysis = tcoAnalysisOf binding' <> tcoAnalysisOf body'
        TcoExpr (tcoNoTailCalls analysis) expr'
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
      let analysis = foldMap (foldMap tcoAnalysisOf) bindings' <> tcoAnalysisOf body'
      TcoExpr (tcoNoTailCalls analysis) expr'
  _ -> do
    let expr' = analyze env <$> expr
    TcoExpr (tcoNoTailCalls (foldMap tcoAnalysisOf expr')) expr'