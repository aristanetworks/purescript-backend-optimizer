module Snapshot.STLoops02 where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef, modify)

test1 :: forall h. STRef h Int -> Int -> Int -> ST h Unit
test1 ref lo hi =
  ST.for (lo + 1) (hi + 1) \a -> do
    void $ modify (_ + a) ref
    void $ modify (_ + a) ref

test2 :: forall h. STRef h Int -> Int -> Int -> ST h Unit
test2 ref lo hi = do
  ST.for (lo + 1) (hi + 1) \a -> void $ modify (_ + a) ref
  ST.for (lo + 1) (hi + 1) (void <<< flip modify ref <<< (+))
  ST.for (lo + 1) (hi + 1) (const (void (modify (_ + 1) ref)))

test3 :: forall h. STRef h Int -> Int -> Int -> ST h Unit
test3 ref lo hi =
  ST.for lo hi \a ->
    when (a < 10) do
      void $ modify (_ + a) ref

test4 :: forall h. STRef h Int -> Int -> Int -> ST h Unit
test4 ref lo hi =
  ST.for lo hi \a ->
    void $
      if a < 10 then
        modify (_ + a) ref
      else
        modify (_ + 1) ref
