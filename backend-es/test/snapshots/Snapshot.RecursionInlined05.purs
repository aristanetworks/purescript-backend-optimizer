-- @inline Snapshot.RecursionInlined05.foldlArray always
-- @inline Snapshot.RecursionInlined05.foldlArray2 always

-- Another "real-world" example from deku
module Snapshot.RecursionInlined05 where

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

test1 :: { count :: Int } -> Int
test1 v = v.count + Array.length (foldlArray2 0 (1) (\b a -> case a of
  Left _ -> b <> [a]
  Right _ -> b
) [] [Left \_ -> Left "hello"])