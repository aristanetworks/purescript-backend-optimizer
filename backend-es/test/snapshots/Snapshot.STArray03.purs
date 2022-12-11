module Snapshot.STArray03 where

import Prelude

import Control.Monad.ST as ST
import Data.Array.ST as STArray

test1 :: forall a b. (a -> b) -> Array a -> Array b
test1 f as = STArray.run do
  bs <- STArray.new
  ST.foreach as \a ->
    void $ STArray.push (f a) bs
  pure bs

test2 :: forall a b. (a -> Array b) -> Array a -> Array b
test2 f as = STArray.run do
  bs <- STArray.new
  ST.foreach as \a -> do
    let as' = f a
    ST.foreach as' (void <<< flip STArray.push bs)
  pure bs
