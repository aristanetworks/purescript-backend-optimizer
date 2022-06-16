module Example7 where

class Loop a where
  loop :: forall b. a -> b

instance Loop Int where
  loop n = loop n

test1 = loop 10