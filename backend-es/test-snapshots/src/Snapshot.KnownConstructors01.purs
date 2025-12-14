module Snapshot.KnownConstructors01 where

import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (const, map)

test1 :: String
test1 = fromMaybe "a" (map (const "b") (Just "c"))
