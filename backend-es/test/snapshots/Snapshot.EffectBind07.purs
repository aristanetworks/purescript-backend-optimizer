module Snapshot.EffectBind07 where

import Prelude

import Effect (Effect)

test :: Effect Int -> (Unit -> Int) -> Effect Int
test random value = do
  x <- random
  n <- do
    let
      a = do
        let
          b = do
            let c = value unit
            c + c
        b + b
    x <- random
    y <- random
    pure $ x + y + a + a
  m <- random
  pure (x + n - m)
