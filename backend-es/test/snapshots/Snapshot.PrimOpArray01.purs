module Snapshot.PrimOpArray01 where

import Data.Array (length, unsafeIndex)
import Data.Array.NonEmpty as NEA
import Partial.Unsafe (unsafePartial)

test1 :: forall a. Array a -> Int
test1 a = length a

test2 :: forall a. Array a -> a
test2 a = unsafePartial unsafeIndex a 2

test3 :: forall a. NEA.NonEmptyArray a -> Int
test3 a = NEA.length a

test4 :: forall a. NEA.NonEmptyArray a -> a
test4 a = unsafePartial NEA.unsafeIndex a 2
