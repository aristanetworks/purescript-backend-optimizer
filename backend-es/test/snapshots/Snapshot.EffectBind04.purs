module Snapshot.EffectBind04 where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

foreign import random :: Effect Int

test :: Effect Unit
test = do
  n <- random
  if n > 100 then
    Console.log "Too hot"
  else if n < 20 then
    Console.log "Too cold"
  else
    Console.log "Just right"
