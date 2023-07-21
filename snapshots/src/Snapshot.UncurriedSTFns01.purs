module Snapshot.UncurriedSTFns01 where

import Prelude

import Control.Monad.ST.Uncurried (STFn3, runSTFn3)
import Control.Monad.ST.Internal (ST)

type F = forall h. STFn3 Int Int Int h Unit
type G = Int -> Int

test1 :: forall h. F -> G -> ST h Unit
test1 f g = runSTFn3 f (g 1) 2 3

test2 :: forall h. F -> G -> Int -> ST h Unit
test2 f g = runSTFn3 f (g 1) 2

test3 :: forall h. F -> G -> Int -> Int -> ST h Unit
test3 f g = runSTFn3 f (g 1)

test4 :: forall h. F -> Int -> Int -> Int -> ST h Unit
test4 f = runSTFn3 f

test5 :: forall h. F -> G -> ST h Unit
test5 f g = do
  runSTFn3 f (g 1) 2 3
  runSTFn3 f (g 1) 2 3

test6 :: forall h. F -> G -> ST h Unit
test6 f g = do
  runSTFn3 f (g 1) 2 3
  runSTFn3 f (g 1) 2 3
  runSTFn3 f (g 1) 2 3

