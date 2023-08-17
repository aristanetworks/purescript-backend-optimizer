module Snapshot.InlineLocalReferenceOpArrayLength where

import Prelude

import Data.Array as Array

test :: ({} -> Int) -> Array Int
test fn = do
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

