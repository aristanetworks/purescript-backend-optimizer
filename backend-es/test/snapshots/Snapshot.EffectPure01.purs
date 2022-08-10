module Snapshot.EffectPure01 where

import Prelude

import Effect (Effect)

foreign import a :: Int

test1 :: Effect Int
test1 = pure 1

test2 :: Effect Int
test2 = pure (a + 1)
