module Snapshot.EffectBind01 where

import Prelude
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: Effect Unit
test1 = do
  Console.log "1"
  value <- Console.log "2"
  Console.log "3"
  pure value
