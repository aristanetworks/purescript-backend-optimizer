module Snapshot.EffectBind03 where

import Prelude

import Effect (Effect)

newtype MyEffect a = MyEffect (Effect a)

instance Functor MyEffect where
  map f (MyEffect a) = MyEffect (map f a)

instance Apply MyEffect where
  apply = ap

instance Applicative MyEffect where
  pure = MyEffect <<< pure

instance Bind MyEffect where
  bind (MyEffect a) k = MyEffect do
    a' <- a
    let (MyEffect k') = k a'
    k'

instance Monad MyEffect

foreign import random :: MyEffect Int

test :: MyEffect Int
test = do
  a <- random
  b <- random
  pure $ a + b
