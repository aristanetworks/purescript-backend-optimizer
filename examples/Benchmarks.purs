module Benchmarks where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Class.Console (log)
import Performance.Minibench (benchWith)

-- node --expose-gc --input-type=module -e "import { main } from \"./output-es/Benchmarks.js\"; main()"

add4 :: Int -> Int -> Int -> Int -> Int
add4 a b c d = a + b + c + d

main :: Effect Unit
main = do
  log "foldl add4 0 to 10000"
  benchWith 1000 (\_ -> foldl (\a b -> add4 a b a b) 0 (0 .. 10000))

  log "add4 partially applied then sum"
  benchWith 1000 \_ ->
     foldl (\a b -> a + b) 0 ((add4 <$> (0 .. 100) <*> (0 .. 100) <*> (0 .. 100) <*> (0 .. 100)) :: Array Int)
