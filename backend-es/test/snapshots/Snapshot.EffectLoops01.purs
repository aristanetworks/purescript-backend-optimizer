module Snapshot.EffectLoops01 where

import Prelude

import Effect (Effect, foreachE)
import Effect.Class.Console as Console

test :: (Int -> Array Int) -> Effect Unit
test k = do
  let x = k 42
  foreachE x \a -> do
    Console.logShow a
    Console.logShow a
