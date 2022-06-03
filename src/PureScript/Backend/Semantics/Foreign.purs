module PureScript.Backend.Semantics.Foreign where

import Prelude

import Data.Lazy (force)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Semantics (BackendNeutral(..), BackendSemantics(..), Env, ExternSpine(..))
import PureScript.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))

type ForeignEval =
  Env -> Qualified Ident -> Array ExternSpine -> Maybe BackendSemantics

type ForeignSemantics =
  Tuple (Qualified Ident) ForeignEval

qualified :: String -> String -> Qualified Ident
qualified mod id = Qualified (Just (ModuleName mod)) (Ident id)

coreForeignSemantics :: Map (Qualified Ident) ForeignEval
coreForeignSemantics = Map.fromFoldable
  [ data_ring_intSub
  , data_semiring_intAdd
  , effect_bindE
  , effect_pureE
  ]

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

data_semiring_intAdd :: ForeignSemantics
data_semiring_intAdd = Tuple (qualified "Data.Semiring" "intAdd") go
  where
  go _ _ = case _ of
    [ ExternApp [ a, b ] ]
      | SemNeutral (NeutLit (LitInt x)) <- Lazy.force a
      , SemNeutral (NeutLit (LitInt y)) <- Lazy.force b ->
        -- TODO: detect overflow
        Just $ SemNeutral (NeutLit (LitInt (x + y)))
    _ ->
      Nothing

data_ring_intSub :: ForeignSemantics
data_ring_intSub = Tuple (qualified "Data.Ring" "intSub") go
  where
  go _ _ = case _ of
    [ ExternApp [ a, b ] ]
      | SemNeutral (NeutLit (LitInt x)) <- Lazy.force a
      , SemNeutral (NeutLit (LitInt y)) <- Lazy.force b ->
        -- TODO: detect overflow
        Just $ SemNeutral (NeutLit (LitInt (x - y)))
    _ ->
      Nothing