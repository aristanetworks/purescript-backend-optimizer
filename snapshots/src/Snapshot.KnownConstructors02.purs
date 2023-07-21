module Snapshot.KnownConstructors02 where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

test :: Either Int Int -> Int
test a = case Just a of
  Just (Left b) -> b
  Just (Right c) -> c
  Nothing -> 42
