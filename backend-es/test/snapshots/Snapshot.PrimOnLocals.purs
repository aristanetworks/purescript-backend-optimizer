-- @inline Snapshot.PrimOnLocals.a always
-- @inline Snapshot.PrimOnLocals.b always
-- @inline Snapshot.PrimOnLocals.c always

module Snapshot.PrimOnLocals where

import Prelude

import Data.Array as Array

a :: forall a. Array a -> Int
a x = Array.length x + (2 * Array.length x)

b :: forall a. Array a -> Int
b x = 42 + 55 + Array.length x

c :: forall a. Array a -> Int
c x = Array.length x

d :: Int
d = a [ 1, 2, 3 ]

e :: Int
e = b [ 4, 5, 6 ]

f :: Int
f = c [ 7, 8, 9 ]
