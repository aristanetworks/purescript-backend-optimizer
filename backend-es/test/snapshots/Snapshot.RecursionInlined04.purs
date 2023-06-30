-- @inline Snapshot.RecursionInlined04.foldlArray always
-- @inline Snapshot.RecursionInlined04.foldlArray2 always

-- A "real-world" example from deku
module Snapshot.RecursionInlined04 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Partial.Unsafe (unsafePartial)

foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldlArray bab b arr = foldlArray2 0 len bab b arr
  where
  len = Array.length arr

foldlArray2 :: forall a b. Int -> Int -> (b -> a -> b) -> b -> Array a -> b
foldlArray2 n i bab b arr
  | n == i = b
  | otherwise = foldlArray2 (n + 1) i bab (bab b (unsafePartial $ Array.unsafeIndex arr n)) arr

test1 :: { left :: Array String, right :: Array String }
test1 = foldlArray
  ( \{ left, right } -> case _ of
      Left x -> { left: left <> [ x ], right }
      Right x -> { left, right: right <> [ x ] }
  )
  { left: [], right: [] }
  [ Left "a" ]
