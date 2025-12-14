module Snapshot.PrimOpIntBit01 where

import Data.Int.Bits

test1 :: Int -> Int -> Int
test1 a b = a .&. b

test2 :: Int -> Int -> Int
test2 a b = a .|. b

test3 :: Int -> Int -> Int
test3 a b = a `shl` b

test4 :: Int -> Int -> Int
test4 a b = a `shr` b

test5 :: Int -> Int -> Int
test5 a b = a `xor` b

test6 :: Int -> Int -> Int
test6 a b = a `zshr` b

test7 :: Int -> Int
test7 a = complement a
