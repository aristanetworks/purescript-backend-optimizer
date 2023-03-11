module Snapshot.CaseRecord where

import Data.Ord ((>))

test1 :: { a :: Int, b :: Int, c :: Int } -> String
test1 = case _ of
  { a: 1 } -> "0"
  { b: 1 } -> "1"
  { c: 1 } -> "2"
  { a: 2, b: 2 } -> "3"
  _ -> "catch"

test2
  :: { a :: { b :: Int, c :: Int }, d :: { e :: Int, f :: Int } }
  -> Int
test2 = case _ of
  { a: { b: 1, c: 2 }, d: { e: 1, f: 2 } } -> 1
  { a: { b: _, c: 2 }, d: { e: 1, f: 2 } } -> 2
  { a: { b: 1, c: 2 }, d: _ } -> 3
  _ -> 4

test3 :: { a :: Int, b :: Int } -> Int
test3 = case _ of
  { a } | a > 0 -> a
  { a: _, b } | b > 1 -> b
  _ -> 3

test4 :: { a :: Int, b :: Int } -> Int
test4 = case _ of
  { a, b: _ } | a > 0 -> a
  { b } | b > 1 -> b
  _ -> 3

test5 :: { a :: Int, b :: Int } -> Int
test5 = case _ of
  { a, b: _ } | a > 0 -> a
  { a: _, b } | b > 0 -> b
  _ -> 0

test6 :: { a :: Int, b :: Int } -> Int
test6 = case _ of
  { a, b }
    | a > 0 -> a
    | b > 0 -> b
  _ -> 0
