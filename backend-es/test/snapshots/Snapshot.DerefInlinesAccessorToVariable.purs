module Snapshot.DerefInlinesAccessorToVariable where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

rec ∷ { a ∷ { a :: Int , b :: Effect Unit } }
rec = { a: { a: 32, b: log "foo" } }

test1 ∷ Tuple { a ∷ { a :: Int , b :: Effect Unit } } Int
test1 = Tuple rec (rec.a.a + 58)