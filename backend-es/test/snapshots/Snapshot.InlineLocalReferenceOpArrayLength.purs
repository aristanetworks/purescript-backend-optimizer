module Snapshot.InlineLocalReferenceOpArrayLength where

import Prelude

import Data.Array as Array

shouldInlineArrayLength :: Array Int -> Array Int
shouldInlineArrayLength = Array.snoc <*> Array.length

test :: Array Int
test = shouldInlineArrayLength [ 1, 2, 3 ]
