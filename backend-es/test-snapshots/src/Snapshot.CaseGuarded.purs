module Snapshot.CaseGuarded where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test1 :: Int -> String
test1 = case _ of
  n
    | n < 1 -> "n: " <> show n
  x
    | x > 1
    , x < 100 -> "1 < x < 100: " <> show x

    | x > 1
    , x < 50 -> "1 < x < 50: " <> show x

    | otherwise -> "catch"

newtype NInt = NInt Int

test2 :: NInt -> Int
test2 = case _ of
  NInt i | i < 1 -> i
  NInt i | i > 1 -> i
  NInt i | i == 1 -> 1
  NInt _ -> 0

data Product3 a b c = Product3 a b c

test3 :: Product3 Int Int Int -> Int
test3 = case _ of
  Product3 a b _ | a == b -> a
  Product3 a b c | c == b -> a
  Product3 a _ c | a == c -> c
  Product3 _ b _ -> b

test4
  :: { a :: Int, b :: Int, c :: Int }
  -> { d :: Int, e :: Int, f :: Int }
  -> Int
test4 = case _, _ of
  { a: 1 }, { d: 1 } -> 1
  _, { d: 2 } -> 2
  _, { d: 3 } -> 3
  { a: 1 }, { d: 4 } -> 4
  { a: 1 }, { d: 5 } -> 5
  { a: 2 }, { d: 1 } -> 6
  { c }, { e, d: 4 }
    | c == e -> 7
    | c < e -> 8
    | c > e -> 9
  { b: 2 }, { d: 1, f: 10 } -> 10
  { c }, { f } -> 11 + c + f

test5 :: Maybe (Either Int Int) -> Int
test5 = case _ of
  Just x
    | Right y <- x -> y
    | Left 2 <- x -> 4
  _ -> 5
