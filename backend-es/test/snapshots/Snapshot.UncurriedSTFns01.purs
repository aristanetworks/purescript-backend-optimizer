module Snapshot.UncurriedSTFns01 where

import Prelude

import Control.Monad.ST.Uncurried (STFn3, runSTFn3)
import Control.Monad.ST.Internal (ST)

foreign import f :: forall a b c r. STFn3 a b c r Unit

foreign import g :: forall a. a -> a

test1 = runSTFn3 f (g 1) 2 3
test2 = runSTFn3 f (g 1) 2
test3 = runSTFn3 f (g 1)
test4 = runSTFn3 f

test5 = do
  runSTFn3 f (g 1) 2 3
  runSTFn3 f (g 1) 2 3

test6 = do
  runSTFn3 f (g 1) 2 3
  runSTFn3 f (g 1) 2 3
  runSTFn3 f (g 1) 2 3

