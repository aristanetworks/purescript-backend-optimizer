module Snapshot.TransformerMaybeT01 where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Random as Random

test1 :: Effect (Maybe Int)
test1 = runMaybeT do
  MaybeT $ map Just $ liftEffect $ Console.log "foo"
  i1 <- MaybeT $ liftEffect $ map Just $ Random.randomInt 1 10
  i2 <- MaybeT $ map (map (_ + 1)) $ liftEffect $ map Just $ Random.randomInt 1 10
  i3 <- MaybeT $ lift2 (+) <$> (liftEffect $ map Just $ Random.randomInt 1 10) <*> (liftEffect $ map Just $ Random.randomInt 1 10)
  pure $ 1 + i1 + i2 + i3

program1 :: forall m. MonadEffect m => MaybeT m Int
program1 = do
  MaybeT $ map Just $ liftEffect $ Console.log "foo"
  i1 <- MaybeT $ liftEffect $ map Just $ Random.randomInt 1 10
  i2 <- MaybeT $ map (map (_ + 1)) $ liftEffect $ map Just $ Random.randomInt 1 10
  i3 <- MaybeT $ lift2 (+) <$> (liftEffect $ map Just $ Random.randomInt 1 10) <*> (liftEffect $ map Just $ Random.randomInt 1 10)
  pure $ 1 + i1 + i2 + i3

test2 :: Effect (Maybe Int)
test2 = runMaybeT program1
