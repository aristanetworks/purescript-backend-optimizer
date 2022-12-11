module Snapshot.STArray01 where

import Data.Array.ST as STArray

test1 :: Array Int
test1 = STArray.run STArray.new
