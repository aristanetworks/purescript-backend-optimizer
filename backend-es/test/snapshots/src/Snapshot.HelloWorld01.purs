module Snapshot.HelloWorld01 where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

test :: Effect Unit
test = Console.log "Hello, World!"
