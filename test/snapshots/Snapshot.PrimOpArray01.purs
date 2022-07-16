module Snapshot.PrimOpArray01 where

import Prelude

import Data.Array (length, unsafeIndex)
import Data.Array.NonEmpty as NEA
import Partial.Unsafe (unsafePartial)

foreign import a :: Array Unit
foreign import b :: NEA.NonEmptyArray Unit

test1 = length a
test2 = unsafePartial unsafeIndex a 2
test3 = NEA.length b
test4 = unsafePartial NEA.unsafeIndex b 2
