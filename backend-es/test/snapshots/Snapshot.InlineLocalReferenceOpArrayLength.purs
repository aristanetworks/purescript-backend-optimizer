module Snapshot.InlineLocalReferenceOpArrayLength where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

test1 :: (forall r. r -> Int) -> Array Int
test1 fn = do
  let
    array =
      [ 1
      , 2
      , fn {}
      ]
  if Array.length array > 3 then
    array
  else
    []

test2 :: (forall r. r -> Array Int) -> Array (Array Int)
test2 fn = do
  let
    array =
      [ [ 1, 2 ]
      , [ 3, 4 ]
      , fn {}
      ]
  if Just 2 == (Array.length <$> Array.index array 1) then
    array
  else
    []
