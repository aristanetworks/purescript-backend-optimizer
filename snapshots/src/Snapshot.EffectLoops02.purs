module Snapshot.EffectLoops02 where

import Prelude

import Effect (Effect, forE)
import Effect.Class.Console as Console

test1 :: Int -> Int -> Effect Unit
test1 lo hi =
  forE (lo + 1) (hi + 1) \a -> do
    Console.logShow a
    Console.logShow a

test2 :: Int -> Int -> Effect Unit
test2 lo hi = do
  forE (lo + 1) (hi + 1) \a -> Console.logShow a
  forE (lo + 1) (hi + 1) Console.logShow
  forE (lo + 1) (hi + 1) (const (Console.log "wat"))

test3 :: Int -> Int -> Effect Unit
test3 lo hi =
  forE lo hi \a ->
    when (a < 10) do
      Console.logShow a

test4 :: Int -> Int -> Effect Unit
test4 lo hi =
  forE lo hi \a ->
    void $
      if a < 10 then
        Console.logShow a
      else
        Console.log "wat"
