module Snapshot.EffectPure01 where

import Prelude

import Effect (Effect)

test1 :: Effect Int
test1 = pure 1

test2 :: Int -> Effect Int
test2 a = pure (a + 1)
