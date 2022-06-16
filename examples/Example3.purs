module Example3 where

import Prelude

import Effect (Effect)

foreign import wat :: forall x. x

foreign import foo :: forall a. Int -> a -> a

foreign import random :: Effect Int

foreign import randomST :: ST Int

newtype ST a = ST (Effect a)

derive newtype instance Functor ST
derive newtype instance Apply ST
derive newtype instance Applicative ST
derive newtype instance Bind ST
derive newtype instance Monad ST

c :: forall a b c. (b -> c) -> (a -> b) -> a -> c
c f g a = f (g a)

k :: forall x y. x -> y -> x
k a _ = a

i :: forall x. x -> x
i x = x

ap :: forall a b. (a -> b) -> a -> b
ap f a = f a

pa :: forall a b. a -> (a -> b) -> b
pa = flip ap

flip :: forall a b c. (a -> b -> c) -> (b -> a -> c)
flip f b a = f a b

-- test1 =
--   c i (c (ap foo wat) (pa 42 foo))

test1 =
  compose (ap foo wat) (compose i (compose (ap foo wat) (pa 42 foo)))

test2 = do
  a <- (\r -> r.foo)
  b <- (\r -> r.bar a)
  (\_ -> foo wat b)


data Either a b = Left a | Right b

either :: forall a b r. (a -> r) -> (b -> r) -> Either a b -> r
either f g = case _ of
  Left a -> f a
  Right b -> g b

arg = Right 12

test3 = ap (either (k (wat 42))) (\b -> foo b wat) arg

test4 = do
  let c = foo 12 wat
  n <- random
  m <- random
  pure (foo n (wat c c))

test5 = do
  let c = foo 12 wat
  n <- randomST
  m <- randomST
  pure (foo n (wat c c))

data Maybe a = Just a | Nothing

test6 = case Just arg of
  Just (Left a) -> a
  Just (Right b) -> b
  Nothing -> 42

test7 = Nothing

go a = go (wat a)

test8 = go 12

-- 1: (let x:1 = 1: ...a in 2: ...b) (1: y) (1: z)

-- let
--   t:1 =
--     let
--       x:1 = 1: ...a
--     in
--       2: ...b
-- in
--   2: t:1 (2: y) (2: z)

-- -- let
-- --   x:1 = 1: ... a
-- --   t:2 = 2: ... b
-- -- in
-- --   3: t:2 (3: y) (3: z)