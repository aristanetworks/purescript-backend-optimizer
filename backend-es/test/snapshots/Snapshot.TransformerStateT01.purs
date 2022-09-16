module Snapshot.TransformerStateT01 where

import Prelude

import Control.Monad.State (class MonadState, get, modify, put, runStateT)
import Control.Monad.State.Trans (StateT)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Random as Random

test1 :: Effect (Tuple String Int)
test1 = 1 # runStateT do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  one <- get
  i2 <- map (_ + one) $ liftEffect $ Random.randomInt 1 10
  put i2
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  result <- modify (_ + i3)
  pure $ show $ i1 + result

program1 :: forall m. MonadEffect m => StateT Int m String
program1 = do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  one <- get
  i2 <- map (_ + one) $ liftEffect $ Random.randomInt 1 10
  put i2
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  result <- modify (_ + i3)
  pure $ show $ i1 + result

test2 :: Effect (Tuple String Int)
test2 = runStateT program1 1

program2 :: forall m. MonadState Int m => MonadEffect m => m String
program2 = do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  one <- get
  i2 <- map (_ + one) $ liftEffect $ Random.randomInt 1 10
  put i2
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  result <- modify (_ + i3)
  pure $ show $ i1 + result

test3 :: Effect (Tuple String Int)
test3 = runStateT program2 2
