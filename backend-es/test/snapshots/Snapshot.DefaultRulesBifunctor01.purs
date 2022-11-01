module Snapshot.DefaultRulesBifunctor01 where

import Prelude

import Data.Bifunctor (lmap, rmap)
import Data.Tuple (Tuple(..))

test1 = lmap show (Tuple true 1)

test2 = rmap show (Tuple true 1)
