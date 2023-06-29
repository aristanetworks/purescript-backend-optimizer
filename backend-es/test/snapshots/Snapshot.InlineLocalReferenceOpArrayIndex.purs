module Snapshot.InlineLocalReferenceOpArrayIndex where

import Prelude

import Data.Array as Array
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

shouldInlineArrayIndex :: Partial => Array Int -> Tuple Int Int
shouldInlineArrayIndex = Tuple <$> flip Array.unsafeIndex 0 <*> flip Array.unsafeIndex 1

test :: Tuple Int Int
test = unsafePartial $ shouldInlineArrayIndex [ 44, 48, 351 ]
