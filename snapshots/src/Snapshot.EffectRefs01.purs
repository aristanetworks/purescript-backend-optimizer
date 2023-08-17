module Snapshot.EffectRefs01 where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

test1 :: Effect (Ref Int)
test1 = Ref.new 42

test2 :: forall a. (Int -> a) -> Effect (Ref a)
test2 g = Ref.new (g 42)

test3 :: forall a. Ref a -> Effect a
test3 r = Ref.read r

test4 :: forall a b. (a -> Ref b) -> a -> Effect b
test4 g r = Ref.read (g r)

test5 :: Ref Int -> Effect Unit
test5 r = Ref.write 42 r

test6 :: forall a. (Int -> a) -> Ref a -> Effect Unit
test6 g r = Ref.write (g 42) r

test7 :: forall a. (a -> a) -> Ref a -> Effect a
test7 g r= Ref.modify g r

test8 :: forall a. (forall b. b -> b) -> Ref a -> Effect a
test8 g r = Ref.modify (g g) r

test9 :: (Int -> Int) -> Effect Int
test9 g = do
  ref <- Ref.new (g 42)
  prev <- Ref.read ref
  Ref.write (prev + 1) ref
  _ <- Ref.modify (add 1) ref
  Ref.read ref
