module Snapshot.STArray04 where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Array.ST as STArray
import Effect (Effect, foreachE)

test1 :: forall a b. (a -> b) -> Array a -> Effect Unit
test1 f as = do
  bs <- liftST $ STArray.new
  foreachE as \a ->
    void $ liftST $ STArray.push (f a) bs
