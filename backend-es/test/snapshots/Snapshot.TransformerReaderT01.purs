-- @inline export test2 always
module Snapshot.TransformerReaderT01 where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, local, runReaderT)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Random as Random

test1 :: Effect Int
test1 = 5 # runReaderT do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 1) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  five <- ask
  i4 <- local (_ * 2) do
    ten <- ask
    liftEffect $ Random.randomInt ten 20
  pure $ 4 + i1 + i2 + i3 + five + i4

test2 :: forall m. MonadReader Int m => MonadEffect m => m Int
test2 = do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 1) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  five <- ask
  i4 <- local (_ * 2) do
    ten <- ask
    liftEffect $ Random.randomInt ten 20
  pure $ 4 + i1 + i2 + i3 + five + i4

test3 :: Effect Int
test3 = runReaderT test2 5

test4 :: forall m. MonadEffect m => Int -> m Int
test4 = runReaderT do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 1) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  five <- ask
  i4 <- local (_ * 2) do
    ten <- ask
    liftEffect $ Random.randomInt ten 20
  pure $ 4 + i1 + i2 + i3 + five + i4

test5 :: Effect Int
test5 = test4 5
