module Snapshot.UncurriedEffectFns02 where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)

test1 :: Effect Unit
test1 = runEffectFn1 (mkEffectFn1 \m -> logShow m) 12

test2 :: Effect Int -> Effect Unit
test2 random = do
  let
    log :: EffectFn1 Int Unit
    log = mkEffectFn1 \n -> logShow n
  n <- random
  runEffectFn1 log n
