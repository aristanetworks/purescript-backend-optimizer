module Snapshot.EffectBind02 where

import Prelude

import Effect (Effect)

newtype MyEffect a = MyEffect (Effect a)

derive newtype instance Functor MyEffect
derive newtype instance Apply MyEffect
derive newtype instance Applicative MyEffect
derive newtype instance Bind MyEffect
derive newtype instance Monad MyEffect

foreign import random :: MyEffect Int

test :: MyEffect Int
test = do
  a <- random
  b <- random
  pure $ a + b
