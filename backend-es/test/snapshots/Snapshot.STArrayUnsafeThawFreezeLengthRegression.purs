module Snapshot.STArrayUnsafeThawFreezeLengthRegression where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STA

-- Related to #92.
-- The call to `Array.length` should not inline to a constant value.

test :: Int -> Array Int
test x = ST.run do
  result <- STA.unsafeThaw $ Array.singleton x
  _ <- STA.push 12 result
  len <- Array.length <$> STA.unsafeFreeze result
  _ <- STA.push len result
  STA.unsafeFreeze result
