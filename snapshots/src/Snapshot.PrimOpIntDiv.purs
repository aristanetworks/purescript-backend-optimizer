-- @inline export divNoInline never
module Snapshot.PrimOpIntDiv where

import Prelude

import Assert (assertEqual)
import Effect (Effect)

divNoInline :: Int -> Int -> Int
divNoInline a b = a / b -- This needs to stay eta-expanded to test runtime div

main :: Effect Unit
main = do
  assertEqual "div1" { expected: 0, actual: divNoInline 1 0 }
  assertEqual "div2" { expected: 1, actual: divNoInline 3 2 }
  assertEqual "div3" { expected: -1, actual: divNoInline 3 (-2) }
