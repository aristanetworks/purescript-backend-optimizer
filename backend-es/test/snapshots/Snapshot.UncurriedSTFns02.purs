module Snapshot.UncurriedSTFns02 where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Control.Monad.ST.Internal (ST, run)
import Control.Monad.ST.Uncurried (STFn1, mkSTFn1, runSTFn1)

foreign import random :: Effect Int

swallow _ = pure unit

test1 :: forall r. ST r Unit
test1 = runSTFn1 (mkSTFn1 \m -> swallow m) 12

test2 :: Effect Unit
test2 = do
  let
    nolog :: forall r. STFn1 Int r Unit
    nolog = mkSTFn1 \n -> swallow n
  n <- random
  logShow (run (runSTFn1 nolog n))
