module Snapshot.HalogenVDomST02 where

import Prelude

import Assert (assertEqual)
import Control.Monad.ST.Class (liftST)
import Data.Array.ST as STArray
import Effect (Effect)
import Effect.Uncurried (mkEffectFn2, mkEffectFn3, runEffectFn5)
import Snapshot.HalogenVDomST01 (diffWithIxE)

main :: Effect Unit
main = do
  merged1 <- liftST $ STArray.new
  added1 <- liftST $ STArray.new
  deleted1 <- liftST $ STArray.new

  result <- runEffectFn5 diffWithIxE
    [ "1", "2", "3" ]
    [ 1, 2 ]
    ( mkEffectFn3 \ix a b -> do
        void $ liftST $ STArray.push { a, b } merged1
        pure { ix, a, b }
    )
    ( mkEffectFn2 \_ a ->
        void $ liftST $ STArray.push a deleted1
    )
    ( mkEffectFn2 \ix b -> do
        void $ liftST $ STArray.push b added1
        pure { ix, a: "", b }
    )

  m1 <- liftST $ STArray.freeze merged1
  a1 <- liftST $ STArray.freeze added1
  d1 <- liftST $ STArray.freeze deleted1

  assertEqual "diffWithIxE/merged"
    { expected: [ { a: "1", b: 1 }, { a: "2", b: 2 } ]
    , actual: m1
    }

  assertEqual "diffWithIxE/added"
    { expected: []
    , actual: a1
    }

  assertEqual "diffWithIxE/deleted"
    { expected: [ "3" ]
    , actual: d1
    }

  assertEqual "diffWithIxE/result"
    { expected: [ { ix: 0, a: "1", b: 1 }, { ix: 1, a: "2", b: 2 } ]
    , actual: result
    }
