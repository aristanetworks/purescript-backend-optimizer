module Snapshot.Tco05 where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

-- minimized from Data.Array
-- let binding in loop
span :: forall a. (a -> Boolean) -> Array a -> Maybe Int
span p arr = go 0
  where
  go i =
    case Array.index arr i of
      Just x -> if p x then go (i + 1) else Just i
      Nothing -> Nothing
