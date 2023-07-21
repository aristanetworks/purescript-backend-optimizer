module Snapshot.CaptureDerefRegression01 where

import Prelude

import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))

data Box a = Box a

test1 :: Tuple Int Int -> Int -> Int
test1 (Tuple a _) b = a + b

test2 :: Tuple Int Int -> Identity (Int -> Int)
test2 (Tuple a _) = Identity \b -> a + b

test3 :: Tuple Int Int -> Box (Int -> Int)
test3 (Tuple a _) = Box \b -> a + b
