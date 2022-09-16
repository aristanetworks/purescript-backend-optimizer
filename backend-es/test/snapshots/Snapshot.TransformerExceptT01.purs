-- @inline export program1 arity=1
-- @inline export program2 arity=2
module Snapshot.TransformerExceptT01 where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT(..), runExceptT, throwError)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Random as Random

test1 :: Effect (Either String Int)
test1 = runExceptT do
  ExceptT $ map Right $ liftEffect $ Console.log "foo"
  i1 <- ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10
  i2 <- map (_ + 4) $ ExceptT $ map (map (_ + 1)) $ liftEffect $ map Right $ Random.randomInt 1 10
  i3 <- (+) <$> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10) <*> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10)
  when (i1 + i2 < i3) do
    throwError "error"
  pure $ 1 + i1 + i2 + i3

program1 :: forall m. MonadEffect m => ExceptT String m Int
program1 = do
  ExceptT $ map Right $ liftEffect $ Console.log "foo"
  i1 <- ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10
  i2 <- map (_ + 4) $ ExceptT $ map (map (_ + 1)) $ liftEffect $ map Right $ Random.randomInt 1 10
  i3 <- (+) <$> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10) <*> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10)
  when (i1 + i2 < i3) do
    throwError "error"
  pure $ 1 + i1 + i2 + i3

test2 :: Effect (Either String Int)
test2 = runExceptT program1

program2 :: forall m. MonadThrow String m => MonadEffect m => m Int
program2 = do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 4) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  when (i1 + i2 < i3) do
    throwError "error"
  pure $ 1 + i1 + i2 + i3

test3 :: Effect (Either String Int)
test3 = runExceptT program2
