module Snapshot.EffectBind06 where

import Prelude

import Effect (Effect)

foreign import random :: Effect Int

test :: Effect Int
test = do
  x <- random
  n <- do
    x <- random
    y <- random
    pure $ x + y
  m <- random
  pure (x + n - m)
