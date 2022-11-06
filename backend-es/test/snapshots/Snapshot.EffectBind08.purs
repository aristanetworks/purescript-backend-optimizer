module Snapshot.EffectBind08 where

import Prelude

import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

test :: Effect Int
test = do
  a <- unsafeCoerce (\_ -> 12)
  pure $ a + 1
