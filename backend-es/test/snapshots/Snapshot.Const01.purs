module Snapshot.Const01 where

import Prelude

import Data.Const (Const(..))
import Data.Eq (eq1)
import Data.Ord (compare1)

test1 = Const "h" == Const "i"
test2 = eq1 (Const "h" :: Const String Int) (Const "i" :: Const String Int)
test3 = Const "h" < Const "i"
test4 = compare1 (Const "h" :: Const String Int) (Const "i" :: Const String Int)
test5 = top :: Const Int Int
test6 = bottom :: Const Int Int
test7 = (Const "h" :: Const String Int) >>> (Const 1 :: Const Int Int)
test8 = (Const "h" :: Const String Int) <> (Const "i" :: Const String Int)
test9 = mempty :: Const String Int
test10 = (Const 1 :: Const Int Int) + (Const 1 :: Const Int Int)
test11 = zero :: Const Int String
test12 = one :: Const Int String
test13 = (Const 1 :: Const Int Int) - (Const 1 :: Const Int Int)
test14 = (Const 1 :: Const Int Int) * (Const 1 :: Const Int Int)
test15 = (Const 1 :: Const Int Int) / (Const 1 :: Const Int Int)
test16 = (Const 2 :: Const Int Int) `mod` (Const 2 :: Const Int Int)
test17 = (Const true :: Const Boolean Int) && (Const true :: Const Boolean Int)
test18 = (Const false :: Const Boolean Int) || (Const false :: Const Boolean Int)
test19 = map show (Const 1 :: Const Int Int)
test20 = apply (Const "h" :: Const String (Int -> String)) (Const "i" :: Const String Int)
test21 = pure 1 :: Const String Int
