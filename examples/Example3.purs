module Example3 where

newtype Foo a = Foo a

test1 (Foo a) = a.foo
test2 a = let b = a in let c = b in c.foo
