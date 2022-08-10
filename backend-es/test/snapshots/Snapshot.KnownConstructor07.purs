module Snapshot.KnownConstructor07 where

import Prelude

foreign import f :: Int -> Int

test :: Int -> { foo :: Int, bar :: Int }
test y = do
  let
    -- Test a regression where poor analysis resulted
    -- in duplicated calls to `f y`.
    z = f y
    a = { foo: z, bar: z }
    b = a { foo = a.foo + 1 }
    c = b { bar = b.bar - 2 }
  c
