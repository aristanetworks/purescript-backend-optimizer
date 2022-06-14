module PureScript.Backend.Semantics.Foreign where

import Prelude

import Data.Array as Array
import Data.Lazy (force)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Semantics (BackendNeutral(..), BackendSemantics(..), Env, ExternSpine(..), evalMkFn, evalPrimOp)
import PureScript.Backend.Syntax (BackendOperator(..), BackendOperatorNum(..), BackendOperatorOrd(..))
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
    [ data_ord_lessThanOrEq
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
      <> map data_function_uncurried_mkFn [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
      <> map data_function_uncurried_runFn [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
      <> map effect_uncurried_mkEffectFn [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
      <> map effect_uncurried_runEffectFn [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

effect_bindE :: ForeignSemantics
effect_bindE = Tuple (qualified "Effect" "bindE") go
  where
  go _ _ = case _ of
    [ ExternApp [ eff, k ] ] | SemLam ident next <- force k ->
      Just $ SemEffectBind ident (force eff) next
    _ -> Nothing

effect_pureE :: ForeignSemantics
effect_pureE = Tuple (qualified "Effect" "pureE") go
  where
  go _ _ = case _ of
    [ ExternApp [ val ] ] ->
      Just $ SemEffectPure (force val)
    _ -> Nothing

data_ord_lessThanOrEq :: ForeignSemantics
data_ord_lessThanOrEq = Tuple (qualified "Data.Ord" "lessThanOrEq") go
  where
  go _ _ = case _ of
    [ ExternApp [ a, b, c ] ]
      | SemExtern (Qualified (Just (ModuleName "Data.Ord")) (Ident "ordInt")) [] _ <- Lazy.force a ->
          Just $ evalPrimOp $ OpIntOrd OpLte (Lazy.force b) (Lazy.force c)
    _ ->
      Nothing

data_semiring_intAdd :: ForeignSemantics
data_semiring_intAdd = Tuple (qualified "Data.Semiring" "intAdd") $ primBinaryOperator (OpIntNum OpAdd)

data_ring_intSub :: ForeignSemantics
data_ring_intSub = Tuple (qualified "Data.Ring" "intSub") $ primBinaryOperator (OpIntNum OpSubtract)

data_function_uncurried_mkFn :: Int -> ForeignSemantics
data_function_uncurried_mkFn n = Tuple (qualified "Data.Function.Uncurried" ("mkFn" <> show n)) go
  where
  go env _ = case _ of
    [ ExternApp [ f ] ] | sem <- Lazy.force f ->
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
          Just $ SemNeutral $ NeutUncurriedApp (Lazy.force head) (Lazy.force <$> tail)
    _ ->
      Nothing

effect_uncurried_mkEffectFn :: Int -> ForeignSemantics
effect_uncurried_mkEffectFn n = Tuple (qualified "Effect.Uncurried" ("mkEffectFn" <> show n)) go
  where
  go env _ = case _ of
    [ ExternApp [ f ] ] | sem <- Lazy.force f ->
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
          Just $ SemNeutral $ NeutUncurriedEffectApp (Lazy.force head) (Lazy.force <$> tail)
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
      | SemNeutral (NeutLit (LitBoolean false)) <- Lazy.force a ->
          Just $ SemNeutral (NeutLit (LitBoolean true))
      | SemNeutral (NeutLit (LitBoolean true)) <- Lazy.force b ->
          Just $ SemNeutral (NeutLit (LitBoolean true))
      | SemNeutral (NeutLit (LitBoolean x)) <- Lazy.force a
      , SemNeutral (NeutLit (LitBoolean y)) <- Lazy.force b ->
          Just $ SemNeutral (NeutLit (LitBoolean (not x || y)))
    _ ->
      Nothing

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

primBinaryOperator :: (BackendSemantics -> BackendSemantics -> BackendOperator BackendSemantics) -> ForeignEval
primBinaryOperator op _ _ = case _ of
    [ ExternApp [ a, b ] ] ->
      Just $ evalPrimOp (op (Lazy.force a) (Lazy.force b))
    _ ->
      Nothing

primUnaryOperator :: (BackendSemantics -> BackendOperator BackendSemantics) -> ForeignEval
primUnaryOperator op _ _ = case _ of
    [ ExternApp [ a ] ] ->
      Just $ evalPrimOp (op (Lazy.force a))
    _ ->
      Nothing
