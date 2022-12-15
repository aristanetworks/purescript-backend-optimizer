module Snapshot.EffectLoops01 where

import Prelude

import Effect (Effect, foreachE)
import Effect.Class.Console as Console

test1 :: (Int -> Array Int) -> Effect Unit
test1 k =
  foreachE (k 42) \a -> do
    Console.logShow a
    Console.logShow a

test2 :: (Int -> Array Int) -> Effect Unit
test2 k = do
  foreachE (k 42) \a -> Console.logShow a
  foreachE (k 42) Console.logShow
  foreachE (k 42) (const (Console.log "wat"))

test3 :: Array Int -> Effect Unit
test3 arr =
  foreachE arr \a ->
    when (a < 10) do
      Console.logShow a

test4 :: Array Int -> Effect Unit
test4 arr =
  foreachE arr \a ->
    void $
      if a < 10 then
        Console.logShow a
      else
        Console.log "wat"
