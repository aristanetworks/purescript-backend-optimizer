module Snapshot.EffectBind05 where

import Prelude

import Effect (Effect)

newtype Id a = Id (Unit -> a)

instance Functor Id where
  map = liftM1

instance Apply Id where
  apply = ap

instance Applicative Id where
  pure a = Id \_ -> a

instance Bind Id where
  bind (Id a) k = k (a unit)

instance Monad Id

test1 :: (Unit -> Effect Unit) -> Effect Unit
test1 k = do
  _ <- pure unit
  k unit

test2 :: (Unit -> Id Unit) -> Id Unit
test2 k = do
  _ <- pure unit
  k unit
