module Snapshot.UncurriedSTFns02 where

import Prelude

import Control.Monad.ST.Internal (ST, run)
import Control.Monad.ST.Uncurried (STFn1, mkSTFn1, runSTFn1)
import Effect (Effect)
import Effect.Class.Console (logShow)

foreign import random :: Effect Int

test1 :: forall r. ST r Int
test1 = runSTFn1 (mkSTFn1 \m -> pure m) 12

test2 :: Effect Unit
test2 = do
  let
    f :: forall r. STFn1 Int r Int
    f = mkSTFn1 \n -> pure n
  n <- random
  logShow (run (runSTFn1 f n))
