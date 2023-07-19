module Snapshot.DerefInlinesAccessorToLocal where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

test1 ∷ Tuple { a ∷ { a :: Int , b :: Effect Unit } } Int
test1 = let rec = { a: { a: 32, b: log "foo" } } in Tuple rec (rec.a.a + 58)
