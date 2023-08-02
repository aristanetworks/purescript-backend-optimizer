module PureScript.Backend.Optimizer.Semantics.Foreign where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), Qualified(..), propKey)
import PureScript.Backend.Optimizer.Semantics (BackendSemantics(..), Env, EvalRef(..), ExternSpine(..), evalAccessor, evalApp, evalAssocOp, evalMkFn, evalPrimOp, evalUncurriedApp, evalUncurriedEffectApp, evalUpdate, liftBoolean, makeLet)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..))

type ForeignEval =
  Env -> Qualified Ident -> Array ExternSpine -> Maybe BackendSemantics

type ForeignSemantics =
  Tuple (Qualified Ident) ForeignEval

qualified :: String -> String -> Qualified Ident
qualified mod id = Qualified (Just (ModuleName mod)) (Ident id)

coreForeignSemantics :: Map (Qualified Ident) ForeignEval
coreForeignSemantics = Map.fromFoldable semantics
  where
  semantics =
    [ control_monad_st_internal_bind
    , control_monad_st_internal_map
    , control_monad_st_internal_modify
    , control_monad_st_internal_new
    , control_monad_st_internal_pure
    , control_monad_st_internal_read
    , control_monad_st_internal_run
    , control_monad_st_internal_write
    , data_array_length
    , data_array_unsafeIndexImpl
    , data_eq_eqBooleanImpl
    , data_eq_eqCharImpl
    , data_eq_eqIntImpl
    , data_eq_eqNumberImpl
    , data_eq_eqStringImpl
    , data_euclideanRing_intDiv
    , data_euclideanRing_numDiv
    , data_heytingAlgebra_boolConj
    , data_heytingAlgebra_boolDisj
    , data_heytingAlgebra_boolNot
    , data_int_bits_and
    , data_int_bits_complement
    , data_int_bits_or
    , data_int_bits_shl
    , data_int_bits_shr
    , data_int_bits_xor
    , data_int_bits_zshr
    , data_ord_ordBoolean
    , data_ord_ordChar
    , data_ord_ordInt
    , data_ord_ordNumber
    , data_ord_ordString
    , data_ring_intSub
    , data_ring_numSub
    , data_semigroup_concatArray
    , data_semigroup_concatString
    , data_semiring_intAdd
    , data_semiring_intMul
    , data_semiring_numAdd
    , data_semiring_numMul
    , data_string_codePoints_toCodePointArray
    , effect_bindE
    , effect_pureE
    , effect_ref_modify
    , effect_ref_new
    , effect_ref_read
    , effect_ref_write
    , effect_unsafe_unsafePerformEffect
    , partial_unsafe_unsafePartial
    , record_builder_copyRecord
    , record_builder_unsafeDelete
    , record_builder_unsafeInsert
    , record_builder_unsafeModify
    , record_builder_unsafeRename
    , record_unsafe_union_unsafeUnionFn
    , record_unsafe_unsafeDelete
    , record_unsafe_unsafeGet
    , record_unsafe_unsafeHas
    , record_unsafe_unsafeSet
    , unsafe_coerce_unsafeCoerce
    ]
      <> map data_function_uncurried_mkFn oneToTen
      <> map data_function_uncurried_runFn oneToTen
      <> map effect_uncurried_mkEffectFn oneToTen
      <> map effect_uncurried_runEffectFn oneToTen
      <> map control_monad_st_uncurried_mkSTFn oneToTen
      <> map control_monad_st_uncurried_runSTFn oneToTen

  oneToTen =
    Array.range 1 10

effect_bindE :: ForeignSemantics
effect_bindE = Tuple (qualified "Effect" "bindE") effectBind

effect_pureE :: ForeignSemantics
effect_pureE = Tuple (qualified "Effect" "pureE") effectPure

effect_ref_new :: ForeignSemantics
effect_ref_new = Tuple (qualified "Effect.Ref" "_new") effectRefNew

effect_ref_read :: ForeignSemantics
effect_ref_read = Tuple (qualified "Effect.Ref" "read") effectRefRead

effect_ref_write :: ForeignSemantics
effect_ref_write = Tuple (qualified "Effect.Ref" "write") effectRefWrite

effect_ref_modify :: ForeignSemantics
effect_ref_modify = Tuple (qualified "Effect.Ref" "modify") effectRefModify

effect_unsafe_unsafePerformEffect :: ForeignSemantics
effect_unsafe_unsafePerformEffect = Tuple (qualified "Effect.Unsafe" "unsafePerformEffect") effectUnsafePerform

control_monad_st_internal_bind :: ForeignSemantics
control_monad_st_internal_bind = Tuple (qualified "Control.Monad.ST.Internal" "bind_") effectBind

control_monad_st_internal_map :: ForeignSemantics
control_monad_st_internal_map = Tuple (qualified "Control.Monad.ST.Internal" "map_") effectMap

control_monad_st_internal_pure :: ForeignSemantics
control_monad_st_internal_pure = Tuple (qualified "Control.Monad.ST.Internal" "pure_") effectPure

control_monad_st_internal_new :: ForeignSemantics
control_monad_st_internal_new = Tuple (qualified "Control.Monad.ST.Internal" "new") effectRefNew

control_monad_st_internal_read :: ForeignSemantics
control_monad_st_internal_read = Tuple (qualified "Control.Monad.ST.Internal" "read") effectRefRead

control_monad_st_internal_run :: ForeignSemantics
control_monad_st_internal_run = Tuple (qualified "Control.Monad.ST.Internal" "run") effectUnsafePerform

control_monad_st_internal_write :: ForeignSemantics
control_monad_st_internal_write = Tuple (qualified "Control.Monad.ST.Internal" "write") effectRefWrite

control_monad_st_internal_modify :: ForeignSemantics
control_monad_st_internal_modify = Tuple (qualified "Control.Monad.ST.Internal" "modify") effectRefModify

data_array_unsafeIndexImpl :: ForeignSemantics
data_array_unsafeIndexImpl = Tuple (qualified "Data.Array" "unsafeIndexImpl") go
  where
  go env _ = case _ of
    [ ExternUncurriedApp [ a, b ] ] ->
      Just $ makeLet Nothing a \a' ->
        makeLet Nothing b \b' ->
          evalPrimOp env (Op2 OpArrayIndex a' b')
    [ ExternApp [ a ] ] ->
      Just $ makeLet Nothing a \a' ->
        SemLam Nothing \b' ->
          evalPrimOp env (Op2 OpArrayIndex a' b')
    _ ->
      Nothing

data_array_length :: ForeignSemantics
data_array_length = Tuple (qualified "Data.Array" "length") $ primUnaryOperator OpArrayLength

data_eq_eqBooleanImpl :: ForeignSemantics
data_eq_eqBooleanImpl = Tuple (qualified "Data.Eq" "eqBooleanImpl") $ primBinaryOperator (OpBooleanOrd OpEq)

data_eq_eqIntImpl :: ForeignSemantics
data_eq_eqIntImpl = Tuple (qualified "Data.Eq" "eqIntImpl") $ primBinaryOperator (OpIntOrd OpEq)

data_eq_eqNumberImpl :: ForeignSemantics
data_eq_eqNumberImpl = Tuple (qualified "Data.Eq" "eqNumberImpl") $ primBinaryOperator (OpNumberOrd OpEq)

data_eq_eqCharImpl :: ForeignSemantics
data_eq_eqCharImpl = Tuple (qualified "Data.Eq" "eqCharImpl") $ primBinaryOperator (OpCharOrd OpEq)

data_eq_eqStringImpl :: ForeignSemantics
data_eq_eqStringImpl = Tuple (qualified "Data.Eq" "eqStringImpl") $ primBinaryOperator (OpStringOrd OpEq)

data_ord_ordBoolean :: ForeignSemantics
data_ord_ordBoolean = Tuple (qualified "Data.Ord" "ordBoolean") $ primOrdOperator OpBooleanOrd

data_ord_ordInt :: ForeignSemantics
data_ord_ordInt = Tuple (qualified "Data.Ord" "ordInt") $ primOrdOperator OpIntOrd

data_ord_ordNumber :: ForeignSemantics
data_ord_ordNumber = Tuple (qualified "Data.Ord" "ordNumber") $ primOrdOperator OpNumberOrd

data_ord_ordChar :: ForeignSemantics
data_ord_ordChar = Tuple (qualified "Data.Ord" "ordChar") $ primOrdOperator OpCharOrd

data_ord_ordString :: ForeignSemantics
data_ord_ordString = Tuple (qualified "Data.Ord" "ordString") $ primOrdOperator OpStringOrd

data_semiring_intAdd :: ForeignSemantics
data_semiring_intAdd = Tuple (qualified "Data.Semiring" "intAdd") $ primBinaryOperator (OpIntNum OpAdd)

data_semiring_intMul :: ForeignSemantics
data_semiring_intMul = Tuple (qualified "Data.Semiring" "intMul") $ primBinaryOperator (OpIntNum OpMultiply)

data_semiring_numAdd :: ForeignSemantics
data_semiring_numAdd = Tuple (qualified "Data.Semiring" "numAdd") $ primBinaryOperator (OpNumberNum OpAdd)

data_semiring_numMul :: ForeignSemantics
data_semiring_numMul = Tuple (qualified "Data.Semiring" "numMul") $ primBinaryOperator (OpNumberNum OpMultiply)

data_ring_intSub :: ForeignSemantics
data_ring_intSub = Tuple (qualified "Data.Ring" "intSub") $ primBinaryOperator (OpIntNum OpSubtract)

data_ring_numSub :: ForeignSemantics
data_ring_numSub = Tuple (qualified "Data.Ring" "numSub") $ primBinaryOperator (OpNumberNum OpSubtract)

data_euclideanRing_intDiv :: ForeignSemantics
data_euclideanRing_intDiv = Tuple (qualified "Data.EuclideanRing" "intDiv") $ primBinaryOperator (OpIntNum OpDivide)

data_euclideanRing_numDiv :: ForeignSemantics
data_euclideanRing_numDiv = Tuple (qualified "Data.EuclideanRing" "numDiv") $ primBinaryOperator (OpNumberNum OpDivide)

data_int_bits_and :: ForeignSemantics
data_int_bits_and = Tuple (qualified "Data.Int.Bits" "and") $ primBinaryOperator OpIntBitAnd

data_int_bits_complement :: ForeignSemantics
data_int_bits_complement = Tuple (qualified "Data.Int.Bits" "complement") $ primUnaryOperator OpIntBitNot

data_int_bits_or :: ForeignSemantics
data_int_bits_or = Tuple (qualified "Data.Int.Bits" "or") $ primBinaryOperator OpIntBitOr

data_int_bits_shl :: ForeignSemantics
data_int_bits_shl = Tuple (qualified "Data.Int.Bits" "shl") $ primBinaryOperator OpIntBitShiftLeft

data_int_bits_shr :: ForeignSemantics
data_int_bits_shr = Tuple (qualified "Data.Int.Bits" "shr") $ primBinaryOperator OpIntBitShiftRight

data_int_bits_xor :: ForeignSemantics
data_int_bits_xor = Tuple (qualified "Data.Int.Bits" "xor") $ primBinaryOperator OpIntBitXor

data_int_bits_zshr :: ForeignSemantics
data_int_bits_zshr = Tuple (qualified "Data.Int.Bits" "zshr") $ primBinaryOperator OpIntBitZeroFillShiftRight

data_function_uncurried_mkFn :: Int -> ForeignSemantics
data_function_uncurried_mkFn n = Tuple (qualified "Data.Function.Uncurried" ("mkFn" <> show n)) go
  where
  go env _ = case _ of
    [ ExternApp [ sem ] ] ->
      Just $ SemMkFn (evalMkFn env n sem)
    _ ->
      Nothing

data_function_uncurried_runFn :: Int -> ForeignSemantics
data_function_uncurried_runFn n = Tuple (qualified "Data.Function.Uncurried" ("runFn" <> show n)) go
  where
  go env _ = case _ of
    [ ExternApp items ]
      | Just { head, tail } <- Array.uncons items ->
          Just $ goRunFn env (n - Array.length tail) head tail
    _ ->
      Nothing

  goRunFn env n' head tail
    | n' <= 0 =
        evalUncurriedApp env head tail
    | otherwise =
        SemLam Nothing \val ->
          goRunFn env (n' - 1) head (Array.snoc tail val)

mkEffectFn :: String -> String -> Int -> ForeignSemantics
mkEffectFn mod name n = Tuple (qualified mod (name <> show n)) go
  where
  go env _ = case _ of
    [ ExternApp [ sem ] ] ->
      Just $ SemMkEffectFn (evalMkFn env n sem)
    _ ->
      Nothing

effect_uncurried_mkEffectFn :: Int -> ForeignSemantics
effect_uncurried_mkEffectFn = mkEffectFn "Effect.Uncurried" "mkEffectFn"

control_monad_st_uncurried_mkSTFn :: Int -> ForeignSemantics
control_monad_st_uncurried_mkSTFn = mkEffectFn "Control.Monad.ST.Uncurried" "mkSTFn"

runEffectFn :: String -> String -> Int -> ForeignSemantics
runEffectFn mod name n = Tuple (qualified mod (name <> show n)) go
  where
  go env _ = case _ of
    [ ExternApp items ]
      | Just { head, tail } <- Array.uncons items
      , Array.length tail == n ->
          Just $ goRunEffectFn env [] head $ List.fromFoldable tail
    _ ->
      Nothing

  goRunEffectFn env acc head = case _ of
    List.Nil ->
      evalUncurriedEffectApp env head acc
    List.Cons arg args ->
      makeLet Nothing arg \nextArg ->
        goRunEffectFn env (Array.snoc acc nextArg) head args

effect_uncurried_runEffectFn :: Int -> ForeignSemantics
effect_uncurried_runEffectFn = runEffectFn "Effect.Uncurried" "runEffectFn"

control_monad_st_uncurried_runSTFn :: Int -> ForeignSemantics
control_monad_st_uncurried_runSTFn = runEffectFn "Control.Monad.ST.Uncurried" "runSTFn"

data_heytingAlgebra_boolConj :: ForeignSemantics
data_heytingAlgebra_boolConj = Tuple (qualified "Data.HeytingAlgebra" "boolConj") $ primBinaryOperator OpBooleanAnd

data_heytingAlgebra_boolDisj :: ForeignSemantics
data_heytingAlgebra_boolDisj = Tuple (qualified "Data.HeytingAlgebra" "boolDisj") $ primBinaryOperator OpBooleanOr

data_heytingAlgebra_boolNot :: ForeignSemantics
data_heytingAlgebra_boolNot = Tuple (qualified "Data.HeytingAlgebra" "boolNot") $ primUnaryOperator OpBooleanNot

data_string_codePoints_toCodePointArray :: ForeignSemantics
data_string_codePoints_toCodePointArray = Tuple (qualified "Data.String.CodePoints" "toCodePointArray") go
  where
  go _ _ = case _ of
    [ ExternApp [ NeutLit (LitString str) ] ] ->
      Just $ NeutLit $ LitArray $ NeutLit <<< LitInt <<< fromEnum <$> String.toCodePointArray str
    _ ->
      Nothing

unsafe_coerce_unsafeCoerce :: ForeignSemantics
unsafe_coerce_unsafeCoerce = Tuple (qualified "Unsafe.Coerce" "unsafeCoerce") go
  where
  go _ _ = case _ of
    [ ExternApp [ a ] ] ->
      Just a
    _ ->
      Nothing

primBinaryOperator :: BackendOperator2 -> ForeignEval
primBinaryOperator op env _ = case _ of
  [ ExternApp [ a ] ] ->
    Just $ makeLet Nothing a \a' ->
      SemLam Nothing \b' ->
        evalPrimOp env (Op2 op a' b')
  _ ->
    Nothing

primUnaryOperator :: BackendOperator1 -> ForeignEval
primUnaryOperator op env _ = case _ of
  [ ExternApp [ a ] ] ->
    Just $ evalPrimOp env (Op1 op a)
  _ ->
    Nothing

primOrdOperator :: (BackendOperatorOrd -> BackendOperator2) -> ForeignEval
primOrdOperator op env _ = case _ of
  [ ExternAccessor (GetProp "compare"), ExternApp [ a, b ], ExternPrimOp (OpIsTag tag) ]
    | isQualified "Data.Ordering" "LT" tag ->
        Just $ evalPrimOp env $ Op2 (op OpLt) a b
    | isQualified "Data.Ordering" "GT" tag ->
        Just $ evalPrimOp env $ Op2 (op OpGt) a b
    | isQualified "Data.Ordering" "EQ" tag ->
        Just $ evalPrimOp env $ Op2 (op OpEq) a b
  _ ->
    Nothing

effectBind :: ForeignEval
effectBind env _ = case _ of
  [ ExternApp [ eff, SemLam ident next ] ] ->
    Just $ makeLet Nothing eff \nextEff ->
      SemEffectBind ident nextEff next
  [ ExternApp [ eff, k ] ] ->
    Just $ makeLet Nothing eff \nextEff ->
      makeLet Nothing k \nextK ->
        SemEffectBind Nothing nextEff \a ->
          evalApp env nextK [ a ]
  _ -> Nothing

effectMap :: ForeignEval
effectMap env _ = case _ of
  [ ExternApp [ fn ] ] ->
    Just $ makeLet Nothing fn \fn' ->
      SemLam Nothing \val ->
        SemEffectBind Nothing val \nextVal ->
          SemEffectPure (evalApp env fn' [ nextVal ])
  _ -> Nothing

effectPure :: ForeignEval
effectPure _ _ = case _ of
  [ ExternApp [ val ] ] ->
    Just $ makeLet Nothing val SemEffectPure
  _ -> Nothing

effectRefNew :: ForeignEval
effectRefNew _ _ = case _ of
  [ ExternApp [ val ] ] ->
    Just $ makeLet Nothing val \val' ->
      NeutPrimEffect $ EffectRefNew val'
  _ -> Nothing

effectRefRead :: ForeignEval
effectRefRead _ _ = case _ of
  [ ExternApp [ val ] ] ->
    Just $ makeLet Nothing val \val' ->
      NeutPrimEffect $ EffectRefRead val'
  _ -> Nothing

effectRefWrite :: ForeignEval
effectRefWrite _ _ = case _ of
  [ ExternApp [ val, ref ] ] ->
    Just $ makeLet Nothing val \val' ->
      makeLet Nothing ref \ref' ->
        NeutPrimEffect $ EffectRefWrite ref' val'
  _ -> Nothing

effectRefModify :: ForeignEval
effectRefModify env _ = case _ of
  [ ExternApp [ fn, ref ] ] ->
    Just $ makeLet Nothing fn \fn' ->
      makeLet Nothing ref \ref' ->
        SemEffectBind Nothing (NeutPrimEffect (EffectRefRead ref')) \val ->
          NeutPrimEffect $ EffectRefWrite ref' (evalApp env fn' [ val ])
  _ ->
    Nothing

effectUnsafePerform :: ForeignEval
effectUnsafePerform _ _ = case _ of
  [ ExternApp [ SemEffectPure a ] ] ->
    Just a
  _ ->
    Nothing

isQualified :: String -> String -> Qualified Ident -> Boolean
isQualified mod tag = case _ of
  Qualified (Just (ModuleName mod')) (Ident tag') ->
    mod == mod' && tag == tag'
  _ ->
    false

data_semigroup_concatArray :: ForeignSemantics
data_semigroup_concatArray = Tuple (qualified "Data.Semigroup" "concatArray") go
  where
  go env qual = case _ of
    [ ExternApp [ NeutLit (LitArray as), NeutLit (LitArray bs) ] ] ->
      Just $ NeutLit (LitArray (as <> bs))
    [ ExternApp [ a, b ] ] ->
      Just $ evalAssocOp env (Left qual) a b
    _ ->
      Nothing

data_semigroup_concatString :: ForeignSemantics
data_semigroup_concatString = Tuple (qualified "Data.Semigroup" "concatString") $ primBinaryOperator OpStringAppend

partial_unsafe_unsafePartial :: ForeignSemantics
partial_unsafe_unsafePartial = Tuple (qualified "Partial.Unsafe" "_unsafePartial") go
  where
  go _ _ = case _ of
    [ ExternApp [ SemLam _ k ] ] ->
      Just $ k (NeutLit (LitRecord []))
    _ ->
      Nothing

record_builder_copyRecord :: ForeignSemantics
record_builder_copyRecord = Tuple (qualified "Record.Builder" "copyRecord") go
  where
  go _ _ = case _ of
    [ ExternApp [ r@(NeutLit (LitRecord _)) ] ] ->
      Just r
    [ ExternApp [ r@(NeutUpdate _ _) ] ] ->
      Just r
    _ ->
      Nothing

record_builder_unsafeInsert :: ForeignSemantics
record_builder_unsafeInsert = Tuple (qualified "Record.Builder" "unsafeInsert") go
  where
  go env _ = case _ of
    [ ExternApp [ NeutLit (LitString prop), value, NeutLit (LitRecord props) ] ] ->
      Just $ NeutLit (LitRecord (Array.snoc props (Prop prop value)))
    [ ExternApp [ NeutLit (LitString prop), value, r@(NeutUpdate _ _) ] ] ->
      Just $ evalUpdate env r [ Prop prop value ]
    [ ExternApp [ NeutLit (LitString prop), value, other ] ] | Just r <- viewCopyRecord other ->
      Just $ evalUpdate env r [ Prop prop value ]
    _ ->
      Nothing

record_builder_unsafeDelete :: ForeignSemantics
record_builder_unsafeDelete = Tuple (qualified "Record.Builder" "unsafeDelete") go
  where
  go _ _ = case _ of
    [ ExternApp [ NeutLit (LitString prop), NeutLit (LitRecord props) ] ] ->
      Just $ NeutLit (LitRecord (Array.filter (not <<< eq prop <<< propKey) props))
    _ ->
      Nothing

record_builder_unsafeModify :: ForeignSemantics
record_builder_unsafeModify = Tuple (qualified "Record.Builder" "unsafeModify") go
  where
  go env _ = case _ of
    [ ExternApp [ NeutLit (LitString prop), fn, NeutLit (LitRecord props) ] ] -> do
      let
        props' = map
          ( \(Prop prop' value) ->
              if prop == prop' then
                Prop prop' (evalApp env fn [ value ])
              else
                Prop prop' value
          )
          props
      Just $ NeutLit (LitRecord props')
    [ ExternApp [ NeutLit (LitString prop), fn, r@(NeutUpdate r'@(NeutLocal _ _) _) ] ] -> do
      let update = Prop prop (evalApp env fn [ (evalAccessor env r' (GetProp prop)) ])
      Just $ evalUpdate env r [ update ]
    [ ExternApp [ NeutLit (LitString prop), fn, other ] ] | Just r <- viewCopyRecord other ->
      Just $ makeLet Nothing r \r' -> do
        let update = Prop prop (evalApp env fn [ (evalAccessor env r' (GetProp prop)) ])
        evalUpdate env r [ update ]
    _ ->
      Nothing

viewCopyRecord :: BackendSemantics -> Maybe BackendSemantics
viewCopyRecord = case _ of
  SemRef (EvalExtern (Qualified (Just (ModuleName "Record.Builder")) (Ident "copyRecord"))) [ ExternApp [ value ] ] _ ->
    Just value
  _ ->
    Nothing

record_builder_unsafeRename :: ForeignSemantics
record_builder_unsafeRename = Tuple (qualified "Record.Builder" "unsafeRename") go
  where
  go _ _ = case _ of
    [ ExternApp [ NeutLit (LitString prop1), NeutLit (LitString prop2), NeutLit (LitRecord props) ] ] -> do
      let
        props' = map
          ( \(Prop prop' value) ->
              if prop1 == prop' then
                Prop prop2 value
              else
                Prop prop' value
          )
          props
      Just $ NeutLit (LitRecord props')
    _ ->
      Nothing

record_unsafe_unsafeGet :: ForeignSemantics
record_unsafe_unsafeGet = Tuple (qualified "Record.Unsafe" "unsafeGet") go
  where
  go env _ = case _ of
    [ ExternApp [ NeutLit (LitString prop) ] ] ->
      Just $ SemLam Nothing \r ->
        evalAccessor env r $ GetProp prop
    _ ->
      Nothing

record_unsafe_unsafeHas :: ForeignSemantics
record_unsafe_unsafeHas = Tuple (qualified "Record.Unsafe" "unsafeHas") go
  where
  go _ _ = case _ of
    [ ExternApp [ NeutLit (LitString prop), NeutLit (LitRecord props) ] ] ->
      Just $ liftBoolean $ Array.any (eq prop <<< propKey) props
    _ ->
      Nothing

record_unsafe_unsafeSet :: ForeignSemantics
record_unsafe_unsafeSet = Tuple (qualified "Record.Unsafe" "unsafeSet") go
  where
  go env _ = case _ of
    [ ExternApp [ NeutLit (LitString prop), value, r ] ] ->
      Just $ evalUpdate env r [ Prop prop value ]
    _ ->
      Nothing

record_unsafe_unsafeDelete :: ForeignSemantics
record_unsafe_unsafeDelete = Tuple (qualified "Record.Unsafe" "unsafeDelete") go
  where
  go _ _ = case _ of
    [ ExternApp [ NeutLit (LitString prop), NeutLit (LitRecord props) ] ] ->
      Just $ NeutLit (LitRecord (Array.filter (not <<< eq prop <<< propKey) props))
    _ ->
      Nothing

record_unsafe_union_unsafeUnionFn :: ForeignSemantics
record_unsafe_union_unsafeUnionFn = Tuple (qualified "Record.Unsafe.Union" "unsafeUnionFn") go
  where
  go _ _ = case _ of
    [ ExternUncurriedApp [ NeutLit (LitRecord props1), NeutLit (LitRecord props2) ] ] ->
      Just $ NeutLit (LitRecord (NonEmptyArray.head <$> Array.groupAllBy (comparing propKey) (props1 <> props2)))
    _ ->
      Nothing
