module Snapshot.EffectMonadEffect01 where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)

main :: forall m. MonadEffect m => m Unit
main = do
  logShow 1
