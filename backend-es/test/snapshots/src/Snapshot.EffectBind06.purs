module Snapshot.EffectBind06 where

import Prelude

import Effect (Effect)

test :: Effect Int -> Effect Int
test random = do
  x <- random
  n <- do
    x <- random  -- Shadowed on purpose
    y <- random
    pure $ x + y
  m <- random
  pure (x + n - m)
