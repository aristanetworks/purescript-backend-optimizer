module Snapshot.STLoops03 where

import Prelude

import Control.Monad.ST (ST, while)
import Control.Monad.ST.Ref (STRef, modify, read)

test1 :: forall h. STRef h Boolean -> STRef h Int -> ST h Unit
test1 cond ref = while (read cond) do
  void $ modify (_ + 1) ref
  void $ modify (_ + 2) ref

test2 :: forall h. STRef h Boolean -> STRef h Int -> ST h Unit
test2 cond ref = do
  while (read cond) do
    void $ modify (_ + 1) ref
  while (read cond) do
    void $ modify (_ + 2) ref

test3 :: forall h. STRef h Boolean -> STRef h Int -> ST h Unit
test3 cond ref = do
  while (read cond) do
    a <- read ref
    when (a < 10) do
      void $ modify (_ + 1) ref

test4 :: forall h. STRef h Boolean -> STRef h Int -> ST h Unit
test4 cond ref = do
  while (read cond) do
    a <- read ref
    void $
      if a < 10 then
        modify (_ + 1) ref
      else
        modify (_ + 2) ref
