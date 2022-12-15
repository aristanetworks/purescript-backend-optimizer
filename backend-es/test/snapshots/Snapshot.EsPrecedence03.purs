module Snapshot.EsPrecedence03 where

import Data.Int.Bits (shr)

test1 :: Int -> Int -> Int
test1 a b = a `shr` b `shr` b

test2 :: Int -> Int -> Int
test2 a b = a `shr` (b `shr` b)
