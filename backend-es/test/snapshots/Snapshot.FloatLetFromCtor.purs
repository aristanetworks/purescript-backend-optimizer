module Snapshot.FloatLetFromCtor where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Console (log)

test1 = Tuple 5
  let
    a = Tuple 1 let b = log "foo" in Tuple b (Tuple 5 b) 
  in
    Tuple a (1 + (fst $ snd $ snd a))
