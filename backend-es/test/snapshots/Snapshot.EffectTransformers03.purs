-- @inline export bindCalls always
-- @inline export discardCalls always
module Snapshot.EffectTransformers03 where

import Prelude

import Control.Monad.Writer.Trans (runWriterT)
import Data.Tuple (snd)
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
test1 = map snd $ runWriterT bindCalls

test2 :: Effect Unit
test2 = map snd $ runWriterT discardCalls

test3 :: Effect Unit
test3 = map snd $ runWriterT do
  _ <- Console.log "hello"
  _ <- Console.logShow 1
  _ <- Console.warn "hello"
  _ <- Console.warnShow 1
  Console.clear

test4 :: Effect Unit
test4 = map snd $ runWriterT do
  Console.log "hello"
  Console.logShow 1
  Console.warn "hello"
  Console.warnShow 1
  Console.clear
