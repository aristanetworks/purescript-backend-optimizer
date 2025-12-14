module Snapshot.EffectBind04 where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

test1 :: Effect Int -> Effect Unit
test1 random = do
  n <- random
  if n > 100 then
    Console.log "Too hot"
  else if n < 20 then
    Console.log "Too cold"
  else
    Console.log "Just right"

test2 :: Effect Int -> Effect Unit
test2 random = do
  n <- random
  if n > 100 then
    Console.log "Too hot"
  else if n < 20 then
    Console.log "Too cold"
  else
    Console.log "Just right"
  Console.log "Done"
