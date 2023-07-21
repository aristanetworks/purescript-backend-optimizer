-- @inline export testArrayIndex never
module Snapshot.InlineArrayIndex where

import Prelude

import Assert (assertEqual)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)

testArrayIndex :: forall a. Array a -> Int -> Maybe a
testArrayIndex arr ix = Array.index arr ix

main :: Effect Unit
main = do
  let array = [ 1, 2, 3 ]
  assertEqual "index -1"
    { expected: Nothing
    , actual: testArrayIndex array (-1)
    }
  assertEqual "index 0"
    { expected: Just 1
    , actual: testArrayIndex array 0
    }
  assertEqual "index 1"
    { expected: Just 2
    , actual: testArrayIndex array 1
    }
  assertEqual "index 2"
    { expected: Just 3
    , actual: testArrayIndex array 2
    }
  assertEqual "index 3"
    { expected: Nothing
    , actual: testArrayIndex array 3
    }
