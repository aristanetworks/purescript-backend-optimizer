-- @inline Data.Ord.ordNumber.compare arity=2
-- @inline Data.Map.Internal.lookup arity=1
-- @inline export lookupInterned never
module PureScript.Backend.Optimizer.Utils where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)
import PureScript.Backend.Optimizer.Interned (Interned)

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

-- This exists to specialize a lookup to primitive comparison operators
-- instead of going through a curried call to compare and Ordering. This
-- gives a modest speed boost for dispatch.
lookupInterned :: forall k v. Interned k -> Map (Interned k) v -> Maybe v
lookupInterned = Map.lookup
