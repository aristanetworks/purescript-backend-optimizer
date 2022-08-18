module PureScript.Backend.Optimizer.Utils where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Partial.Unsafe (unsafePartial)

foldl1Array :: forall a b. (b -> a -> b) -> (a -> b) -> NonEmptyArray a -> b
foldl1Array f g arr = go 1 (g (NonEmptyArray.head arr))
  where
  len = NonEmptyArray.length arr
  go ix acc
    | ix == len = acc
    | otherwise =
        go (ix + 1) (f acc (unsafePartial (NonEmptyArray.unsafeIndex arr ix)))

foldr1Array :: forall a b. (a -> b -> b) -> (a -> b) -> NonEmptyArray a -> b
foldr1Array f g arr = go (NonEmptyArray.length arr - 2) (g (NonEmptyArray.last arr))
  where
  go ix acc
    | ix < 0 = acc
    | otherwise =
        go (ix - 1) (f (unsafePartial (NonEmptyArray.unsafeIndex arr ix)) acc)
