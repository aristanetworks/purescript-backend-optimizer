module Snapshot.EffectLoops01 where

import Prelude

import Effect (Effect, foreachE)
import Effect.Class.Console as Console

test :: Effect Unit
test = do
  pure unit
  foreachE [1, 2, 3] \a -> do
    Console.logShow a
    Console.logShow a
  pure unit
