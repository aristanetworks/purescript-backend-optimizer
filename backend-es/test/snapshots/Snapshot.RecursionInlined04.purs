-- @inline Snapshot.RecursionInlined04.foldlArray always
-- @inline Snapshot.RecursionInlined04.foldlArray2 always

-- A "real-world" example from deku
module Snapshot.RecursionInlined04 where

import Prelude

import Data.Array as Array
import Partial.Unsafe (unsafePartial)

foldlArray :: forall a b . (b -> a -> b) -> b -> Array a -> b
foldlArray bab b arr = foldlArray2 0 len bab b arr
  where
  len = Array.length arr

foldlArray2 :: forall a b . Int -> Int -> (b -> a -> b) -> b -> Array a -> b
foldlArray2 n i bab b arr
  | n == i - 1 = b
  | otherwise = foldlArray2 (n + 1) i bab (bab b (unsafePartial $ Array.unsafeIndex arr n)) arr
test1 :: String
test1 = foldlArray (<>) "" ["a", "b", "c"]