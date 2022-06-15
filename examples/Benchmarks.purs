module Benchmarks where

import Prelude

import Effect (Effect, forE)

-- node --expose-gc --input-type=module -e "import { main } from \"./output-es/Benchmarks.js\"; main()"

foreign import benchmark :: forall a. String -> Int -> Effect a -> Effect Unit

data TestNumber = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten

data SixTuple a = SixTuple a a a a a a

addNumber6 :: TestNumber -> TestNumber -> TestNumber -> TestNumber -> TestNumber -> TestNumber -> TestNumber
addNumber6 a b c d e f = do
  let
    g = case a, b of
      One, Two -> Two
      _, _ -> Ten

  case g, c, d of
    Two, Two, Two -> e
    Ten, Two, One -> f
    _, _, _ -> Ten

testNumbers :: Array TestNumber
testNumbers = [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten ]

main :: Effect Unit
main = do
  benchmark "Repeated applications of 2 arguments" 3000 do
    forE 0 3000 \_ -> do
      let g = addNumber6 Five One
      let h = g Two Four
      let ans = h Nine Ten
      pure ans $> unit

  benchmark "Fully saturated application of 6 arguments" 3000 do
    forE 0 3000 \_ -> do
      let ans = addNumber6 Five Five Eight Three Nine Two
      pure ans $> unit
