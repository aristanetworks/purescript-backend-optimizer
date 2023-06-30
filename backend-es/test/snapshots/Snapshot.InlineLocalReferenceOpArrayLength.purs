module Snapshot.InlineLocalReferenceOpArrayLength where

import Prelude

import Data.Array as Array

shouldInlineArrayLength :: Array Int -> Array Int
shouldInlineArrayLength = Array.snoc <*> Array.length

b :: Array Int
b = shouldInlineArrayLength [ 1, 2, 3 ]
