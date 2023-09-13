module Snapshot.FloatLetFromApp where

import Prelude

import Control.Apply (lift2)
import Data.Array (foldl)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

test1 ∷ Tuple Int (Tuple (Either (Effect Unit) Int) (Either (Effect Unit) Int))
test1 = Tuple 1
  let
    a = foldl (lift2 (+)) (Right 3) let b = log "foo" in [Left b, Right 4, Left b]
  in
    Tuple a ((_ + 1) <$> a)
