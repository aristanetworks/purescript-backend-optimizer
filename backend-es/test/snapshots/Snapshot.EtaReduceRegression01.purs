module Snapshot.EtaReduceRegression01 where

import Prelude

import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe)

fold :: forall f a. Foldable f => Monoid a => f a -> a
fold = foldMap identity

test :: Maybe String -> String
test = fold
