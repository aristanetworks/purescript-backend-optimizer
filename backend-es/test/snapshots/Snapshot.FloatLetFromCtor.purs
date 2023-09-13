module Snapshot.FloatLetFromCtor where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

test1 :: Tuple Int (Tuple (Tuple Int (Tuple (Effect Unit) (Tuple Int (Effect Unit)))) Int)
test1 = Tuple 5
  let
    a = Tuple 1 let b = log "foo" in Tuple b (Tuple 5 b)
  in
    Tuple a (1 + (fst $ snd $ snd a))
