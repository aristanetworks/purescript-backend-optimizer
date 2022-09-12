-- @inline export bindCalls always
-- @inline export discardCalls always
module Snapshot.EffectBind07 where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console

bindCalls :: forall m. MonadEffect m => m Unit
bindCalls = do
  _ <- Console.log "hello"
  _ <- Console.logShow 1
  _ <- Console.warn "hello"
  _ <- Console.warnShow 1
  Console.clear

discardCalls :: forall m. MonadEffect m => m Unit
discardCalls = do
  Console.log "hello"
  Console.logShow 1
  Console.warn "hello"
  Console.warnShow 1
  Console.clear

test1 :: Effect Unit
test1 = bindCalls

test2 :: Effect Unit
test2 = discardCalls

test3 :: Effect Unit
test3 = do
  _ <- Console.log "hello"
  _ <- Console.logShow 1
  _ <- Console.warn "hello"
  _ <- Console.warnShow 1
  Console.clear

test4 :: Effect Unit
test4 = do
  Console.log "hello"
  Console.logShow 1
  Console.warn "hello"
  Console.warnShow 1
  Console.clear
