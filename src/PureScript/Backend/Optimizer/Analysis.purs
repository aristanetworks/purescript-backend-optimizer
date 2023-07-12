module PureScript.Backend.Optimizer.Analysis where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as SCU
import Data.Traversable (foldMap, foldr)
import Data.Tuple (Tuple(..), snd)
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Qualified)
import PureScript.Backend.Optimizer.Syntax (class HasSyntax, BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendSyntax(..), Level, Pair(..), sndPair, syntaxOf)

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
  , update :: Int
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
    , update: a.update + b.update
    }

instance Monoid Usage where
  mempty = Usage
    { total: 0
    , captured: mempty
    , arities: Set.empty
    , call: 0
    , access: 0
    , case: 0
    , update: 0
    }

data Complexity = Trivial | Deref | KnownSize | NonTrivial

derive instance Eq Complexity
derive instance Ord Complexity

instance Semigroup Complexity where
  append = max

instance Monoid Complexity where
  mempty = Trivial

data ResultTerm = KnownNeutral | Unknown

derive instance Eq ResultTerm

instance Semigroup ResultTerm where
  append = case _, _ of
    Unknown, _ -> Unknown
    _, Unknown -> Unknown
    _, _ -> KnownNeutral

instance Monoid ResultTerm where
  mempty = KnownNeutral

newtype BackendAnalysis = BackendAnalysis
  { usages :: Map Level Usage
  , size :: Int
  , complexity :: Complexity
  , args :: Array Usage
  , rewrite :: Boolean
  , deps :: Set (Qualified Ident)
  , result :: ResultTerm
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
    , result: a.result <> b.result
    }

instance Monoid BackendAnalysis where
  mempty = BackendAnalysis
    { usages: Map.empty
    , size: 0
    , complexity: Trivial
    , args: []
    , rewrite: false
    , deps: Set.empty
    , result: KnownNeutral
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
    { usages = Map.singleton level $ Usage
        { total: 1
        , captured: mempty
        , arities: Set.empty
        , call: 0
        , access: 0
        , case: 0
        , update: 0
        }
    }

accessed :: Level -> BackendAnalysis -> BackendAnalysis
accessed level (BackendAnalysis s) = do
  BackendAnalysis s
    { usages = Map.update (Just <<< over Usage (\us -> us { access = us.access + 1 })) level s.usages
    }

cased :: Level -> BackendAnalysis -> BackendAnalysis
cased level (BackendAnalysis s) = do
  BackendAnalysis s
    { usages = Map.update (Just <<< over Usage (\us -> us { case = us.case + 1 })) level s.usages
    }

updated :: Level -> BackendAnalysis -> BackendAnalysis
updated level (BackendAnalysis s) = do
  BackendAnalysis s
    { usages = Map.update (Just <<< over Usage (\us -> us { update = us.update + 1 })) level s.usages
    }

usedDep :: Qualified Ident -> BackendAnalysis
usedDep dep = do
  let BackendAnalysis s = mempty
  BackendAnalysis s { deps = Set.singleton dep }

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

withResult :: ResultTerm -> BackendAnalysis -> BackendAnalysis
withResult r (BackendAnalysis s) = BackendAnalysis s { result = r }

class HasAnalysis a where
  analysisOf :: a -> BackendAnalysis

resultOf :: forall a. HasAnalysis a => a -> ResultTerm
resultOf = analysisOf >>> unwrap >>> _.result

analyze :: forall a. HasAnalysis a => HasSyntax a => (Tuple (Qualified Ident) (Maybe BackendAccessor) -> BackendAnalysis) -> BackendSyntax a -> BackendAnalysis
analyze externAnalysis expr = case expr of
  Var qi -> do
    let BackendAnalysis { args } = externAnalysis (Tuple qi Nothing)
    withArgs args
      $ bump
      $ usedDep qi
  Local _ lvl ->
    bump
      $ used lvl
  Let _ lvl a b ->
    withResult (resultOf b)
      $ bump
      $ complex NonTrivial
      $ analysisOf a <> bound lvl (analysisOf b)
  LetRec lvl as b ->
    withResult (resultOf b)
      $ complex NonTrivial
      $ bound lvl
      $ bump
      $ foldMap (analysisOf <<< snd) as <> analysisOf b
  EffectBind _ lvl a b ->
    withResult Unknown
      $ complex NonTrivial
      $ capture CaptureClosure
      $ bump
      $ analysisOf a <> bound lvl (analysisOf b)
  EffectPure a ->
    withResult Unknown
      $ capture CaptureClosure
      $ bump
      $ analysisOf a
  EffectDefer a ->
    withResult Unknown
      $ capture CaptureClosure
      $ bump
      $ analysisOf a
  Abs args _ ->
    withResult KnownNeutral
      $ complex KnownSize
      $ capture CaptureClosure
      $ foldr (boundArg <<< snd) (analyzeDefault expr) args
  UncurriedAbs args _ ->
    withResult KnownNeutral
      $ complex KnownSize
      $ capture CaptureClosure
      $ foldr (boundArg <<< snd) (analyzeDefault expr) args
  UncurriedApp hd tl ->
    case syntaxOf hd of
      Just (Local _ lvl) ->
        callArity lvl (Array.length tl) analysis
      _ ->
        analysis
    where
    analysis =
      withResult Unknown
        $ complex NonTrivial
        $ analyzeDefault expr
  UncurriedEffectAbs args _ ->
    withResult KnownNeutral
      $ complex KnownSize
      $ capture CaptureClosure
      $ foldr (boundArg <<< snd) (analyzeDefault expr) args
  UncurriedEffectApp hd tl ->
    case syntaxOf hd of
      Just (Local _ lvl) ->
        callArity lvl (Array.length tl) analysis
      _ ->
        analysis
    where
    analysis =
      withResult Unknown
        $ complex NonTrivial
        $ capture CaptureClosure
        $ analyzeDefault expr
  App hd tl | BackendAnalysis { args } <- analysisOf hd ->
    withArgs remainingArgs case syntaxOf hd of
      Just (Local _ lvl) ->
        withResult Unknown
          $ callArity lvl (NonEmptyArray.length tl)
          $ bump
          $ analysis
      _ ->
        withResult Unknown
          $ bump
          $ analysis
    where
    remainingArgs =
      Array.drop (NonEmptyArray.length tl) args
    analysis
      | Array.null remainingArgs =
          complex NonTrivial $ analyzeDefault expr
      | otherwise =
          analyzeDefault expr
  Update hd _ ->
    case syntaxOf hd of
      Just (Local _ lvl) ->
        updated lvl analysis
      _ ->
        analysis
    where
    analysis =
      withResult Unknown
        $ complex NonTrivial
        $ analyzeDefault expr
  CtorSaturated qi _ _ _ cs ->
    withResult KnownNeutral
      $ bump
      $ foldMap (foldMap analysisOf) cs <> usedDep qi
  CtorDef _ _ _ _ ->
    complex NonTrivial $ analyzeDefault expr
  Branch bs def -> do
    let Pair a b = NonEmptyArray.head bs
    let result = foldMap (resultOf <<< sndPair) bs
    withResult result $ complex NonTrivial do
      analysisOf a
        <> capture CaptureBranch (analysisOf b)
        <> capture CaptureBranch (foldMap (foldMap analysisOf) (NonEmptyArray.tail bs))
        <> capture CaptureBranch (analysisOf def)
  Fail _ ->
    complex NonTrivial
      $ analyzeDefault expr
  PrimOp op ->
    case op of
      Op1 (OpIsTag _) b | Just (Local _ lvl) <- syntaxOf b ->
        cased lvl analysis
      _ ->
        analysis
    where
    analysis =
      withResult Unknown
        $ complex NonTrivial
        $ analyzeDefault expr
  PrimEffect _ ->
    withResult Unknown
      $ complex NonTrivial
      $ capture CaptureClosure
      $ analyzeDefault expr
  PrimUndefined ->
    analyzeDefault expr
  Accessor hd acc ->
    case syntaxOf hd of
      Just (Accessor _ (GetCtorField qi _ _ _ _ _)) ->
        analysis <> usedDep qi
      Just (Accessor _ _) ->
        analysis
      Just (Local _ lvl) ->
        accessed lvl $ complex Deref analysis
      Just (Var qi) -> do
        let BackendAnalysis { args } = externAnalysis (Tuple qi (Just acc))
        withArgs args $ complex Trivial analysis
      _ ->
        complex Deref analysis
    where
    analysis =
      withResult Unknown
        $ analyzeDefault expr
  Lit lit ->
    case lit of
      LitArray as | Array.length as > 0 ->
        complex KnownSize analysis
      LitRecord ps
        | Array.length ps > 0 ->
            complex KnownSize analysis
        | otherwise ->
            analysis
      LitString str | SCU.length str > 128 ->
        complex KnownSize analysis
      _ ->
        analysis
    where
    analysis =
      withResult KnownNeutral
        $ analyzeDefault expr

analyzeEffectBlock :: forall a. HasAnalysis a => HasSyntax a => (Tuple (Qualified Ident) (Maybe BackendAccessor) -> BackendAnalysis) -> BackendSyntax a -> BackendAnalysis
analyzeEffectBlock externAnalysis expr = case expr of
  Let _ lvl a b ->
    withResult (resultOf b)
      $ complex NonTrivial
      $ bump
      $ analysisOf a <> bound lvl (analysisOf b)
  LetRec lvl as b ->
    withResult (resultOf b)
      $ complex NonTrivial
      $ bound lvl
      $ bump
      $ foldMap (analysisOf <<< snd) as <> analysisOf b
  EffectBind _ lvl a b ->
    withResult Unknown
      $ complex NonTrivial
      $ bump
      $ analysisOf a <> bound lvl (analysisOf b)
  EffectPure a ->
    withResult Unknown
      $ bump
      $ analysisOf a
  EffectDefer a ->
    withResult Unknown
      $ bump
      $ analysisOf a
  UncurriedEffectApp hd tl ->
    case syntaxOf hd of
      Just (Local _ lvl) ->
        callArity lvl (Array.length tl) analysis
      _ ->
        analysis
    where
    analysis =
      withResult Unknown
        $ complex NonTrivial
        $ analyzeDefault expr
  PrimEffect _ ->
    withResult Unknown
      $ complex NonTrivial
      $ analyzeDefault expr
  _ ->
    analyze externAnalysis expr

analyzeDefault :: forall a. HasAnalysis a => BackendSyntax a -> BackendAnalysis
analyzeDefault = bump <<< foldMap analysisOf
