module PureScript.Backend.Semantics.Foreign where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Semantics (BackendSemantics(..), Env, ExternSpine(..), evalMkFn, evalPrimOp)
import PureScript.Backend.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..))
import PureScript.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))

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
    [ data_ord_ordInt
    , data_ord_ordNumber
    , data_ord_ordString
    , data_ord_ordChar
    , data_ord_ordBoolean
    , data_ring_intSub
    , data_semiring_intAdd
    , effect_bindE
    , effect_pureE
    , data_heytingAlgebra_boolConj
    , data_heytingAlgebra_boolDisj
    , data_heytingAlgebra_boolNot
    , data_heytingAlgebra_boolImplies
    , data_eq_eqBooleanImpl
    , data_eq_eqIntImpl
    , data_eq_eqNumberImpl
    , data_eq_eqCharImpl
    , data_eq_eqStringImpl
    ]
      <> map data_function_uncurried_mkFn oneToTen
      <> map data_function_uncurried_runFn oneToTen
      <> map effect_uncurried_mkEffectFn oneToTen
      <> map effect_uncurried_runEffectFn oneToTen

  oneToTen =
    Array.range 1 10

effect_bindE :: ForeignSemantics
effect_bindE = Tuple (qualified "Effect" "bindE") go
  where
  go _ _ = case _ of
    [ ExternApp [ eff, SemLam ident next ] ] ->
      Just $ SemEffectBind ident eff next
    _ -> Nothing

effect_pureE :: ForeignSemantics
effect_pureE = Tuple (qualified "Effect" "pureE") go
  where
  go _ _ = case _ of
    [ ExternApp [ val ] ] ->
      Just $ SemEffectPure val
    _ -> Nothing

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

data_ring_intSub :: ForeignSemantics
data_ring_intSub = Tuple (qualified "Data.Ring" "intSub") $ primBinaryOperator (OpIntNum OpSubtract)

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
  go _ _ = case _ of
    [ ExternApp items ]
      | Just { head, tail } <- Array.uncons items
      , Array.length tail == n ->
          Just $ NeutUncurriedApp head tail
    _ ->
      Nothing

effect_uncurried_mkEffectFn :: Int -> ForeignSemantics
effect_uncurried_mkEffectFn n = Tuple (qualified "Effect.Uncurried" ("mkEffectFn" <> show n)) go
  where
  go env _ = case _ of
    [ ExternApp [ sem ] ] ->
      Just $ SemMkEffectFn (evalMkFn env n sem)
    _ ->
      Nothing

effect_uncurried_runEffectFn :: Int -> ForeignSemantics
effect_uncurried_runEffectFn n = Tuple (qualified "Effect.Uncurried" ("runEffectFn" <> show n)) go
  where
  go _ _ = case _ of
    [ ExternApp items ]
      | Just { head, tail } <- Array.uncons items
      , Array.length tail == n ->
          Just $ NeutUncurriedEffectApp head tail
    _ ->
      Nothing

data_heytingAlgebra_boolConj :: ForeignSemantics
data_heytingAlgebra_boolConj = Tuple (qualified "Data.HeytingAlgebra" "boolConj") $ primBinaryOperator OpBooleanAnd

data_heytingAlgebra_boolDisj :: ForeignSemantics
data_heytingAlgebra_boolDisj = Tuple (qualified "Data.HeytingAlgebra" "boolDisj") $ primBinaryOperator OpBooleanOr

data_heytingAlgebra_boolNot :: ForeignSemantics
data_heytingAlgebra_boolNot = Tuple (qualified "Data.HeytingAlgebra" "boolNot") $ primUnaryOperator OpBooleanNot

data_heytingAlgebra_boolImplies :: ForeignSemantics
data_heytingAlgebra_boolImplies = Tuple (qualified "Data.HeytingAlgebra" "boolImplies") go
  where
  go _ _ = case _ of
    [ ExternApp [ a, b ] ]
      | NeutLit (LitBoolean false) <- a ->
          Just $ NeutLit (LitBoolean true)
      | NeutLit (LitBoolean true) <- b ->
          Just $ NeutLit (LitBoolean true)
      | NeutLit (LitBoolean x) <- a
      , NeutLit (LitBoolean y) <- b ->
          Just $ NeutLit (LitBoolean (not x || y))
    _ ->
      Nothing

primBinaryOperator :: BackendOperator2 -> ForeignEval
primBinaryOperator op env _ = case _ of
    [ ExternApp [ a, b ] ] ->
      Just $ evalPrimOp env (Op2 op a b)
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

isQualified :: String -> String -> Qualified Ident-> Boolean
isQualified mod tag = case _ of
  Qualified (Just (ModuleName mod')) (Ident tag') ->
    mod == mod' && tag == tag'
  _ ->
    false