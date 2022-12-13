module Snapshot.EffectBind09 where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

when' :: Boolean -> (Unit -> Effect Unit) -> Effect Unit
when' bool k = if bool then k unit else pure unit

test1 :: Boolean -> Effect Unit
test1 bool = do
  when' bool \_ -> Console.log "1"
  when' bool \_ -> Console.log "2"
  when' bool \_ -> Console.log "3"
