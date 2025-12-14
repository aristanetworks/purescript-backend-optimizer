module Snapshot.CaseLeafTco where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

test1 :: Boolean -> Array Int -> Array Int
test1 b arr = case Array.head arr, Array.last arr of
  Just 1, Just 2 -> arr
  Nothing, Just y -> arr <> [ y ]
  Nothing, Nothing -> arr
  Just x, Nothing -> arr <> [ x ]
  Just x, Just y ->
    if b then
      []
    else
      test1 b ([ y, x, 3, y, 5, 6, 7, 8, 9, 10, x, 12, 13, 14, 15, 16, 17 ] <> arr)
