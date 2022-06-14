module PureScript.Backend.Syntax where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldlDefault, foldrDefault, sequenceDefault, traverse)
import Data.Tuple (Tuple)
import PureScript.CoreFn (Ident, Literal(..), Prop, Qualified)

data BackendSyntax a
  = Var (Qualified Ident)
  | Local (Maybe Ident) Level
  | Lit (Literal a)
  | App a (NonEmptyArray a)
  | Abs (NonEmptyArray (Tuple (Maybe Ident) Level)) a
  | UncurriedApp a (Array a)
  | UncurriedAbs (Array (Tuple (Maybe Ident) Level)) a
  | UncurriedEffectApp a (Array a)
  | UncurriedEffectAbs (Array (Tuple (Maybe Ident) Level)) a
  | Accessor a BackendAccessor
  | Update a (Array (Prop a))
  | CtorSaturated (Qualified Ident) Ident (Array (Tuple Ident a))
  | CtorDef Ident (Array Ident)
  | LetRec Level (Array (Tuple Ident a)) a
  | Let (Maybe Ident) Level a a
  | EffectBind (Maybe Ident) Level a a
  | EffectPure a
  | Branch (Array (Pair a)) (Maybe a)
  | PrimOp (BackendOperator a)
  | Test a BackendGuard
  | Fail String

newtype Level = Level Int

derive newtype instance Eq Level
derive newtype instance Ord Level
derive instance Newtype Level _

data Pair a = Pair a a

data BackendAccessor
  = GetProp String
  | GetIndex Int
  | GetOffset Int

data BackendGuard
  = GuardNumber Number
  | GuardInt Int
  | GuardString String
  | GuardBoolean Boolean
  | GuardChar Char
  | GuardTag (Qualified Ident)
  | GuardArrayLength Int

data BackendOperator a
  = OpBooleanAnd a a
  | OpBooleanNot a
  | OpBooleanOr a a
  | OpBooleanOrd BackendOperatorOrd a a
  | OpCharOrd BackendOperatorOrd a a
  | OpIntBitAnd a a
  | OpIntBitNot a
  | OpIntBitOr a a
  | OpIntBitShiftLeft a a
  | OpIntBitShiftRight a a
  | OpIntBitXor a a
  | OpIntBitZeroFillShiftRight a a
  | OpIntNegate a
  | OpIntNum BackendOperatorNum a a
  | OpIntOrd BackendOperatorOrd a a
  | OpNumberNegate a
  | OpNumberNum BackendOperatorNum a a
  | OpNumberOrd BackendOperatorOrd a a
  | OpStringAppend a a
  | OpStringOrd BackendOperatorOrd a a

data BackendOperatorNum = OpAdd | OpDivide | OpMultiply | OpSubtract

data BackendOperatorOrd = OpEq | OpGt | OpGte | OpLt | OpLte

derive instance Functor BackendSyntax

instance Foldable BackendSyntax where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    Var _ -> mempty
    Local _ _ -> mempty
    Lit lit ->
      case lit of
        LitArray as -> foldMap f as
        LitRecord as -> foldMap (foldMap f) as
        _ -> mempty
    App a bs -> f a <> foldMap f bs
    Abs _ b -> f b
    UncurriedApp a bs -> f a <> foldMap f bs
    UncurriedAbs _ b -> f b
    UncurriedEffectApp a bs -> f a <> foldMap f bs
    UncurriedEffectAbs _ b -> f b
    Accessor a _ -> f a
    Update a bs -> f a <> foldMap (foldMap f) bs
    LetRec _ as b -> foldMap (foldMap f) as <> f b
    Let _ _ b c -> f b <> f c
    EffectBind _ _ b c -> f b <> f c
    EffectPure a -> f a
    Branch as b -> foldMap (foldMap f) as <> foldMap f b
    Test a _ -> f a
    PrimOp a -> foldMap f a
    CtorSaturated _ _ cs -> foldMap (foldMap f) cs
    CtorDef _ _ -> mempty
    Fail _ -> mempty

instance Traversable BackendSyntax where
  sequence a = sequenceDefault a
  traverse f = case _ of
    Var a ->
      pure (Var a)
    Local a b ->
      pure (Local a b)
    Lit lit ->
      case lit of
        LitInt a -> pure (Lit (LitInt a))
        LitNumber a -> pure (Lit (LitNumber a))
        LitString a -> pure (Lit (LitString a))
        LitChar a -> pure (Lit (LitChar a))
        LitBoolean a -> pure (Lit (LitBoolean a))
        LitArray as -> Lit <<< LitArray <$> traverse f as
        LitRecord as -> Lit <<< LitRecord <$> traverse (traverse f) as
    App a bs ->
      App <$> f a <*> traverse f bs
    Abs as b ->
      Abs as <$> f b
    UncurriedApp a bs ->
      UncurriedApp <$> f a <*> traverse f bs
    UncurriedAbs as b ->
      UncurriedAbs as <$> f b
    UncurriedEffectApp a bs ->
      UncurriedEffectApp <$> f a <*> traverse f bs
    UncurriedEffectAbs as b ->
      UncurriedEffectAbs as <$> f b
    Accessor a b ->
      flip Accessor b <$> f a
    Update a bs ->
      Update <$> f a <*> traverse (traverse f) bs
    CtorDef a bs ->
      pure (CtorDef a bs)
    CtorSaturated a b cs ->
      CtorSaturated a b <$> traverse (traverse f) cs
    LetRec lvl as b ->
      LetRec lvl <$> traverse (traverse f) as <*> f b
    Let ident lvl b c ->
      Let ident lvl <$> f b <*> f c
    EffectBind ident lvl b c ->
      EffectBind ident lvl <$> f b <*> f c
    EffectPure a ->
      EffectPure <$> f a
    Branch as b ->
      Branch <$> traverse (traverse f) as <*> traverse f b
    Test a b ->
      flip Test b <$> f a
    PrimOp a ->
      PrimOp <$> traverse f a
    Fail a ->
      pure (Fail a)

derive instance Functor Pair

instance Foldable Pair where
  foldl f acc (Pair a b) = f (f acc a) b
  foldr f acc (Pair a b) = f a (f b acc)
  foldMap f (Pair a b) = f a <> f b

instance Traversable Pair where
  sequence a = sequenceDefault a
  traverse f (Pair a b) = Pair <$> f a <*> f b

derive instance Functor BackendOperator

instance Foldable BackendOperator where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    OpBooleanAnd a b -> f a <> f b
    OpBooleanNot a -> f a
    OpBooleanOr a b -> f a <> f b
    OpBooleanOrd _ a b -> f a <> f b
    OpCharOrd _ a b -> f a <> f b
    OpIntBitAnd a b -> f a <> f b
    OpIntBitNot a -> f a
    OpIntBitOr a b -> f a <> f b
    OpIntBitShiftLeft a b -> f a <> f b
    OpIntBitShiftRight a b -> f a <> f b
    OpIntBitXor a b -> f a <> f b
    OpIntBitZeroFillShiftRight a b -> f a <> f b
    OpIntNegate a -> f a
    OpIntNum _ a b -> f a <> f b
    OpIntOrd _ a b -> f a <> f b
    OpNumberNegate a -> f a
    OpNumberNum _ a b -> f a <> f b
    OpNumberOrd _ a b -> f a <> f b
    OpStringAppend a b -> f a <> f b
    OpStringOrd _ a b -> f a <> f b

instance Traversable BackendOperator where
  sequence a = sequenceDefault a
  traverse f = case _ of
    OpBooleanAnd a b -> OpBooleanAnd <$> f a <*> f b
    OpBooleanNot a -> OpBooleanNot <$> f a
    OpBooleanOr a b -> OpBooleanOr <$> f a <*> f b
    OpBooleanOrd op a b -> OpBooleanOrd op <$> f a <*> f b
    OpCharOrd op a b -> OpCharOrd op <$> f a <*> f b
    OpIntBitAnd a b -> OpIntBitAnd <$> f a <*> f b
    OpIntBitNot a -> OpIntBitNot <$> f a
    OpIntBitOr a b -> OpIntBitOr <$> f a <*> f b
    OpIntBitShiftLeft a b -> OpIntBitShiftLeft <$> f a <*> f b
    OpIntBitShiftRight a b -> OpIntBitShiftRight <$> f a <*> f b
    OpIntBitXor a b -> OpIntBitXor <$> f a <*> f b
    OpIntBitZeroFillShiftRight a b -> OpIntBitZeroFillShiftRight <$> f a <*> f b
    OpIntNegate a -> OpIntNegate <$> f a
    OpIntNum op a b -> OpIntNum op <$> f a <*> f b
    OpIntOrd op a b -> OpIntOrd op <$> f a <*> f b
    OpNumberNegate a -> OpNumberNegate <$> f a
    OpNumberNum op a b -> OpNumberNum op <$> f a <*> f b
    OpNumberOrd op a b -> OpNumberOrd op <$> f a <*> f b
    OpStringAppend a b -> OpStringAppend <$> f a <*> f b
    OpStringOrd op a b -> OpStringOrd op <$> f a <*> f b

class HasSyntax a where
  syntaxOf :: a -> Maybe (BackendSyntax a)
