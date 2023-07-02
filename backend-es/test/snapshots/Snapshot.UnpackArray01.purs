module Snapshot.UnpackArray01 where

import Data.Array as Array
import Partial.Unsafe (unsafePartial)

test :: (String -> String -> String) -> String
test fn = unsafePartial do
  let
    array =
      [ "foo"
      , "bar"
      , "baz"
      ]
  fn (Array.unsafeIndex array 0) (Array.unsafeIndex array 2)
