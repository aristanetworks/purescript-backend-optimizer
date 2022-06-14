module Benchmarks where

import Prelude

import Effect (Effect)
import Performance.Minibench (benchWith)

-- node --expose-gc --input-type=module -e "import { main } from \"./output-es/Benchmarks.js\"; main()"

data Nine = Nine Int Int Int Int Int Int Int Int Int

addNine :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
addNine a b c d e f g h i
  | a < 10 = a
  | b < 10 = b
  | c < 10 = c
  | d < 10 = d
  | e < 10 = e
  | f < 10 = f
  | g < 10 = g
  | h < 10 = h
  | i < 10 = i
  | otherwise = a

testData :: Unit -> Array Nine
testData _ = go 1000 []
  where
  go n acc
    | n <= 0 = acc
    | otherwise = go (n - 1) (acc <> [ Nine n n n n n n n n n ])

test1 :: Array Nine -> Array (Int -> Int -> Int -> Int -> Int -> Int -> Int)
test1 = map (\(Nine a b c _ _ _ _ _ _) -> addNine a b c)

test2 :: Array (Int -> Int -> Int -> Int -> Int -> Int -> Int) -> Array (Int -> Int -> Int -> Int)
test2 = map (\f -> f 20 30 40)

test3 :: Array (Int -> Int -> Int -> Int) -> Array Int
test3 = map (\f -> f 50 60 70)

test4 :: Array Nine -> Array Int
test4 = map (\(Nine a b c d e f g h i) -> addNine a b c d e f g h i)

main :: Effect Unit
main = do
  let items = testData unit

  -- Partially apply a function of 9 arguments 3 at a time
  benchWith 1000 \_ -> do
     let a = test1 items
     let b = test2 a
     test3 b

  -- Apply a function of 9 arguments all at once
  benchWith 1000 \_ -> test4 items
