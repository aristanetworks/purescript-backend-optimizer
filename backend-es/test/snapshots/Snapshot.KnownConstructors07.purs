module Snapshot.KnownConstructors07 where

import Prelude

test :: (Int -> Int) -> Int -> { foo :: Int, bar :: Int }
test f y = do
  let
    -- Test a regression where poor analysis resulted
    -- in duplicated calls to `f y`.
    z = f y
    a = { foo: z, bar: z }
    b = a { foo = a.foo + 1 }
    c = b { bar = b.bar - 2 }
  c
