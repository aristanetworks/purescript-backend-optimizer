module Snapshot.EffectLoopCaseRegression where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect, foreachE)
import Effect.Console as Console

test :: Effect (Maybe (Array String)) -> Effect Unit
test eff = do
  res <- eff
  case res of
    Nothing ->
      pure unit
    Just as ->
      foreachE as \a ->
        Console.log a
