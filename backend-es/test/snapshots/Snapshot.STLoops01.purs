module Snapshot.STLoops01 where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef, modify)

test1 :: forall h. STRef h Int -> (Int -> Array Int) -> ST h Unit
test1 ref k =
  ST.foreach (k 42) \a -> do
    void $ modify (_ + a) ref
    void $ modify (_ + a) ref

test2 :: forall h. STRef h Int -> (Int -> Array Int) -> ST h Unit
test2 ref k = do
  ST.foreach (k 42) \a -> void $ modify (_ + a) ref
  ST.foreach (k 42) (void <<< flip modify ref <<< (+))
  ST.foreach (k 42) (const (void (modify (_ + 1) ref)))

test3 :: forall h. STRef h Int -> Array Int -> ST h Unit
test3 ref arr =
  ST.foreach arr \a ->
    when (a < 10) do
      void $ modify (_ + a) ref

test4 :: forall h. STRef h Int -> Array Int -> ST h Unit
test4 ref arr =
  ST.foreach arr \a ->
    void $
      if a < 10 then
        modify (_ + a) ref
      else
        modify (_ + 1) ref
