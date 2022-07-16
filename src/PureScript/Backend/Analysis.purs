module PureScript.Backend.Analysis where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (foldMap, foldr)
import Data.Tuple (Tuple(..), snd)
import PureScript.Backend.Syntax (class HasSyntax, BackendOperator(..), BackendOperator1(..), BackendSyntax(..), Level, syntaxOf)
import PureScript.CoreFn (Ident, Literal(..), ModuleName, Qualified(..))

data Capture = CaptureNone | CaptureBranch | CaptureClosure

derive instance Eq Capture
derive instance Ord Capture

instance Semigroup Capture where
  append = max

instance Monoid Capture where
  mempty = CaptureNone

newtype Usage = Usage
  { total :: Int
  , captured :: Capture
  , arities :: Set Int
  , call :: Int
  , access :: Int
  , case :: Int
  }

derive instance Newtype Usage _

instance Semigroup Usage where
  append (Usage a) (Usage b) = Usage
    { total: a.total + b.total
    , captured: a.captured <> b.captured
    , arities: Set.union a.arities b.arities
    , call: a.call + b.call
    , access: a.access + b.access
    , case: a.case + b.case
    }

instance Monoid Usage where
  mempty = Usage { total: 0, captured: mempty, arities: Set.empty, call: 0, access: 0, case: 0 }

data Complexity = Trivial | Deref | Known | NonTrivial

derive instance Eq Complexity
derive instance Ord Complexity

instance Semigroup Complexity where
  append = max

instance Monoid Complexity where
  mempty = Trivial

newtype BackendAnalysis = BackendAnalysis
  { usages :: Map Level Usage
  , size :: Int
  , complexity :: Complexity
  , args :: Array Usage
  , rewrite :: Boolean
  , deps :: Set ModuleName
  }

derive instance Newtype BackendAnalysis _

instance Semigroup BackendAnalysis where
  append (BackendAnalysis a) (BackendAnalysis b) = BackendAnalysis
    { usages: Map.unionWith append a.usages b.usages
    , size: a.size + b.size
    , complexity: a.complexity <> b.complexity
    , args: []
    , rewrite: a.rewrite || b.rewrite
    , deps: Set.union a.deps b.deps
    }

instance Monoid BackendAnalysis where
  mempty = BackendAnalysis
    { usages: Map.empty
    , size: 0
    , complexity: Trivial
    , args: []
    , rewrite: false
    , deps: Set.empty
    }

bound :: Level -> BackendAnalysis -> BackendAnalysis
bound level (BackendAnalysis s) = BackendAnalysis s { usages = Map.delete level s.usages }

boundArg :: Level -> BackendAnalysis -> BackendAnalysis
boundArg level (BackendAnalysis s) = case Map.pop level s.usages of
  Nothing ->
    BackendAnalysis s { args = Array.cons mempty s.args }
  Just (Tuple u us) ->
    BackendAnalysis s { usages = us, args = Array.cons u s.args }

withArgs :: Array Usage -> BackendAnalysis -> BackendAnalysis
withArgs args (BackendAnalysis s) = BackendAnalysis s { args = args }

withRewrite :: BackendAnalysis -> BackendAnalysis
withRewrite (BackendAnalysis s) = BackendAnalysis s { rewrite = true }

used :: Level -> BackendAnalysis
used level = do
  let BackendAnalysis s = mempty
  BackendAnalysis s
    { usages = Map.singleton level (Usage { total: 1, captured: mempty, arities: Set.empty, call: 0, access: 0, case: 0 })
    }

accessed :: Level -> BackendAnalysis -> BackendAnalysis
accessed level (BackendAnalysis s) = do
  BackendAnalysis s
    { usages = Map.update (Just <<< over Usage (\us -> us { access = us.access + 1 })) level  s.usages
    }

cased :: Level -> BackendAnalysis -> BackendAnalysis
cased level (BackendAnalysis s) = do
  BackendAnalysis s
    { usages = Map.update (Just <<< over Usage (\us -> us { case = us.case + 1 })) level  s.usages
    }

usedDep :: ModuleName -> BackendAnalysis
usedDep mn = do
  let BackendAnalysis s = mempty
  BackendAnalysis s { deps = Set.singleton mn }

bump :: BackendAnalysis -> BackendAnalysis
bump (BackendAnalysis s) = BackendAnalysis s { size = s.size + 1 }

complex :: Complexity -> BackendAnalysis -> BackendAnalysis
complex complexity (BackendAnalysis s) = BackendAnalysis s { complexity = s.complexity <> complexity }

capture :: Capture -> BackendAnalysis -> BackendAnalysis
capture cap (BackendAnalysis s) = BackendAnalysis s { usages = over Usage _ { captured = cap } <$> s.usages }

callArity :: Level -> Int -> BackendAnalysis -> BackendAnalysis
callArity lvl arity (BackendAnalysis s) = BackendAnalysis s
  { usages = Map.update (Just <<< over Usage (\us -> us { arities = Set.insert arity us.arities, call = us.call + 1 })) lvl s.usages
  }

class HasAnalysis a where
  analysisOf :: a -> BackendAnalysis

analyze :: forall a. HasAnalysis a => HasSyntax a => (Qualified Ident -> BackendAnalysis) -> BackendSyntax a -> BackendAnalysis
analyze externAnalysis expr = case expr of
  Var qi@(Qualified mn _) -> do
    let BackendAnalysis { args } = externAnalysis qi
    withArgs args $ bump $ foldMap usedDep mn
  Local _ lvl ->
    bump (used lvl)
  Let _ lvl a b ->
    bump (complex NonTrivial (analysisOf a <> bound lvl (analysisOf b)))
  LetRec lvl as b ->
    bump (complex NonTrivial (bound lvl (foldMap (analysisOf <<< snd) as <> analysisOf b)))
  EffectBind _ lvl a b ->
    bump (complex NonTrivial (analysisOf a <> capture CaptureClosure (bound lvl (analysisOf b))))
  EffectPure a ->
    capture CaptureClosure $ bump (analysisOf a)
  Abs args _ ->
    complex NonTrivial $ capture CaptureClosure $ foldr (boundArg <<< snd) (analyzeDefault expr) args
  UncurriedAbs args _ ->
    complex NonTrivial $ capture CaptureClosure $ foldr (boundArg <<< snd) (analyzeDefault expr) args
  UncurriedApp hd tl ->
    case syntaxOf hd of
      Just (Local _ lvl) ->
        callArity lvl (Array.length tl) analysis
      _ ->
        analysis
    where
    analysis = complex NonTrivial $ analyzeDefault expr
  UncurriedEffectAbs args _ ->
    complex NonTrivial $ capture CaptureClosure $ foldr (boundArg <<< snd) (analyzeDefault expr) args
  UncurriedEffectApp hd tl ->
    case syntaxOf hd of
      Just (Local _ lvl) ->
        callArity lvl (Array.length tl) analysis
      _ ->
        analysis
    where
    analysis = complex NonTrivial $ analyzeDefault expr
  App hd tl | BackendAnalysis { args } <- analysisOf hd ->
    withArgs remainingArgs case syntaxOf hd of
      Just (Local _ lvl) ->
        callArity lvl (NonEmptyArray.length tl) analysis
      _ ->
        analysis
    where
    remainingArgs =
      Array.drop (NonEmptyArray.length tl) args
    analysis
      | Array.null remainingArgs =
          complex NonTrivial $ analyzeDefault expr
      | otherwise =
          analyzeDefault expr
  Update _ _ ->
    complex NonTrivial $ analyzeDefault expr
  CtorSaturated (Qualified mn _) _ _ _ cs ->
    bump (foldMap (foldMap analysisOf) cs <> foldMap usedDep mn)
  CtorDef _ _ _ _ ->
    complex NonTrivial $ analyzeDefault expr
  Branch _ _ ->
    complex NonTrivial $ capture CaptureBranch $ analyzeDefault expr
  Fail _ ->
    complex NonTrivial $ analyzeDefault expr
  PrimOp op ->
    case op of
      Op1 (OpIsTag _) b | Just (Local _ lvl) <- syntaxOf b ->
        cased lvl analysis
      _ ->
        analysis
    where
    analysis = complex NonTrivial $ analyzeDefault expr
  PrimEffect _ ->
    complex NonTrivial $ analyzeDefault expr
  Accessor hd _ ->
    case syntaxOf hd of
      Just (Accessor _ _) ->
        analysis
      Just (Local _ lvl) ->
        accessed lvl analysis
      Just (Var _) ->
        complex Trivial analysis
      _ ->
        complex Deref analysis
    where
    analysis = analyzeDefault expr
  Lit lit ->
    case lit of
      LitArray as | Array.length as > 0 ->
        complex Known analysis
      LitRecord ps | Array.length ps > 0 ->
        complex Known analysis
      LitString _ ->
        complex Known analysis
      _ ->
        complex Trivial analysis
    where
    analysis = analyzeDefault expr

analyzeEffectBlock :: forall a. HasAnalysis a => HasSyntax a => (Qualified Ident -> BackendAnalysis) -> BackendSyntax a -> BackendAnalysis
analyzeEffectBlock externAnalysis expr = case expr of
  Let _ lvl a b ->
    bump (complex NonTrivial (analysisOf a <> bound lvl (analysisOf b)))
  LetRec lvl as b ->
    bump (complex NonTrivial (bound lvl (foldMap (analysisOf <<< snd) as <> analysisOf b)))
  EffectBind _ lvl a b ->
    bump (complex NonTrivial (analysisOf a <> bound lvl (analysisOf b)))
  EffectPure a ->
    bump (analysisOf a)
  _ ->
    analyze externAnalysis expr

analyzeDefault :: forall a. HasAnalysis a => BackendSyntax a -> BackendAnalysis
analyzeDefault = bump <<< foldMap analysisOf
