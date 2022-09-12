module Snapshot.EffectConsole01 where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Writer.Trans (runWriterT)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: Effect Unit
test1 = do
  Console.log "hello"
  Console.logShow 1
  Console.warn "hello"
  Console.warnShow 1
  Console.error "hello"
  Console.errorShow 1
  Console.info "hello"
  Console.infoShow 1
  Console.debug "hello"
  Console.debugShow 1
  Console.time "start"
  Console.timeLog "middle"
  Console.timeEnd "finish"
  Console.clear

test2 :: Effect Unit
test2 = "input" # runReaderT do
  Console.log "hello"
  Console.logShow 1
  Console.warn "hello"
  Console.warnShow 1
  Console.error "hello"
  Console.errorShow 1
  Console.info "hello"
  Console.infoShow 1
  Console.debug "hello"
  Console.debugShow 1
  Console.time "start"
  Console.timeLog "middle"
  Console.timeEnd "finish"
  Console.clear

test3 :: Effect Unit
test3 = map snd $ runWriterT do
  Console.log "hello"
  Console.logShow 1
  Console.warn "hello"
  Console.warnShow 1
  Console.error "hello"
  Console.errorShow 1
  Console.info "hello"
  Console.infoShow 1
  Console.debug "hello"
  Console.debugShow 1
  Console.time "start"
  Console.timeLog "middle"
  Console.timeEnd "finish"
  Console.clear
