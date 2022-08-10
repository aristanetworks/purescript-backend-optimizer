module Snapshot.KnownConstructors02 where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

foreign import a :: Either Int Int

test :: Int
test = case Just a of
  Just (Left b) -> b
  Just (Right c) -> c
  Nothing -> 42
