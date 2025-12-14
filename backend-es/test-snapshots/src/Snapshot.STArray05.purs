module Snapshot.STArray05 (test) where

import Prelude

import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Array.ST as STArray

f :: forall h. (STArray h Int -> ST h Int) -> ST h (STArray h Int)
f eff = do
  arr <- STArray.new
  _ <- eff arr
  pure arr

test :: Boolean -> Array Int
test x = STArray.run (f (if x then \a -> STArray.push 1 a else \a -> STArray.unshift 2 a))
