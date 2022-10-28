module Snapshot.STRun01 where

import Prelude

import Control.Monad.ST (run)
import Control.Monad.ST.Ref (new, read)

test1 :: Int
test1 = run (pure 1)

test2 :: Int
test2 = run do
  n <- pure 1
  m <- pure 2
  pure (n + m)

test3 :: Int
test3 = run do
  nRef <- new 1
  mRef <- new 2
  n <- read nRef
  m <- read mRef
  pure (n + m)
