module PureScript.Backend.Utils where

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
