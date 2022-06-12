module Run where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Effect.Class.Console

-- Run this with
-- node --input-type=module -e "import { main } from \"./output-es/Run.js\"; main()"
-- after
-- yarn run test-examples

main :: Effect Unit
main = do
  Effect.Class.Console.log "Hello, world!"
