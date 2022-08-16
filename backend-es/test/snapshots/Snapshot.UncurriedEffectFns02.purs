module Snapshot.UncurriedEffectFns02 where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)

foreign import random :: Effect Int

test1 :: Effect Unit
test1 = runEffectFn1 (mkEffectFn1 \m -> logShow m) 12

test2 :: Effect Unit
test2 = do
  let
    log :: EffectFn1 Int Unit
    log = mkEffectFn1 \n -> logShow n
  n <- random
  runEffectFn1 log n
