module Snapshot.EffectBind04 where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

foreign import random :: Effect Int

test1 :: Effect Unit
test1 = do
  n <- random
  if n > 100 then
    Console.log "Too hot"
  else if n < 20 then
    Console.log "Too cold"
  else
    Console.log "Just right"

test2 :: Effect Unit
test2 = do
  n <- random
  if n > 100 then
    Console.log "Too hot"
  else if n < 20 then
    Console.log "Too cold"
  else
    Console.log "Just right"
  Console.log "Done"
