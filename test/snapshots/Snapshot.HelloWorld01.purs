module Snapshot.HelloWorld01 where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

main :: Effect Unit
main = Console.log "Hello, World!"
