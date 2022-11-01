-- @inline export program1 arity=1
-- @inline export program2 arity=1
module Snapshot.TransformerReaderT02 where

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

program1 :: forall m. MonadReader Int m => MonadEffect m => m Int
program1 = do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 1) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  five <- ask
  i4 <- local (_ * 2) do
    ten <- ask
    liftEffect $ Random.randomInt ten 20
  pure $ 4 + i1 + i2 + i3 + five + i4

test2 :: Effect Int
test2 = runReaderT program1 5

program2 :: forall m. MonadEffect m => Int -> m Int
program2 = runReaderT do
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
test3 = program2 5
