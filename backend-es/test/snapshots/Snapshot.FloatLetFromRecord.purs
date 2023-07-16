module Snapshot.FloatLetFromRecord where

import Prelude

import Effect (Effect)
import Effect.Console (log)

test1
  :: { l :: Int
     , r ::
         { l ::
             { l :: Int
             , r ::
                 { l :: Effect Unit
                 , r ::
                     { l :: Int
                     , r :: Effect Unit
                     }
                 }
             }
         , r :: Int
         }
     }
test1 =
  { l: 5
  , r:
      let
        a = { l: 1, r: let b = log "foo" in { l: b, r: { l: 5, r: b } } }
      in
        { l: a, r: 1 + (a.r.r.l) }
  }
