module Snapshot.EffectBind02 where

import Prelude

import Effect (Effect)

newtype MyEffect a = MyEffect (Effect a)

derive newtype instance Functor MyEffect
derive newtype instance Apply MyEffect
derive newtype instance Applicative MyEffect
derive newtype instance Bind MyEffect
derive newtype instance Monad MyEffect

test :: MyEffect Int -> MyEffect Int
test random = do
  a <- random
  b <- random
  pure $ a + b
