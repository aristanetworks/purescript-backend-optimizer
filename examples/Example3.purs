module Example3 where

foreign import wat :: forall x. x

foreign import foo :: forall a. Int -> a -> a

class Bind f where
  bind :: forall a b. f a -> (a -> f b) -> f b

instance Bind (Function r) where
  bind k1 k2 r = k2 (k1 r) r

c :: forall a b c. (b -> c) -> (a -> b) -> a -> c
c f g a = f (g a)

k :: forall x y. x -> y -> x
k a _ = a

i :: forall x. x -> x
i x = x

ap :: forall a b. (a -> b) -> a -> b
ap f a = f a

pa :: forall a b. a -> (a -> b) -> b
pa a f = f a

test1 =
  c i (c (ap foo wat) (pa 42 foo))

test2 = do
  a <- (\r -> r.foo)
  b <- (\r -> r.bar a)
  (\_ -> foo wat b)


data Either a b = Left a | Right b

either :: forall a b r. (a -> r) -> (b -> r) -> Either a b -> r
either f g = case _ of
  Left a -> f a
  Right b -> g b

test3 = either (k (wat 42)) (\b -> foo b wat)