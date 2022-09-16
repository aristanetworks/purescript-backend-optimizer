-- @inline export program1 arity=1
module Snapshot.TransformerExceptT02 where

import Prelude

import Control.Monad.Except.Trans (class MonadError, ExceptT(..), catchError, runExceptT, throwError)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Random as Random

test1 :: Effect (Either String Int)
test1 = runExceptT do
  ExceptT $ map Right $ liftEffect $ Console.log "foo"
  i1 <- ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10
  when (i1 < 5) do
    throwError "oh no!"
  i2 <- map (_ + 4) $ ExceptT $ map (map (_ + 1)) $ liftEffect $ map Right $ Random.randomInt 1 10
  i3 <- (+) <$> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10) <*> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10)
  i4 <- flip catchError (\e -> if i2 < 5 then pure 8 else throwError e) do
    i5 <- ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10
    when (i5 < 5) do
      throwError "below 5"
    pure i5
  pure $ 1 + i1 + i2 + i3 + i4

program1 :: forall m. MonadEffect m => ExceptT String m Int
program1 = do
  ExceptT $ map Right $ liftEffect $ Console.log "foo"
  i1 <- ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10
  i2 <- map (_ + 4) $ ExceptT $ map (map (_ + 1)) $ liftEffect $ map Right $ Random.randomInt 1 10
  i3 <- (+) <$> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10) <*> (ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10)
  i4 <- flip catchError (\e -> if i2 < 5 then pure 8 else throwError e) do
    i5 <- ExceptT $ liftEffect $ map Right $ Random.randomInt 1 10
    when (i5 < 5) do
      throwError "below 5"
    pure i5
  pure $ 1 + i1 + i2 + i3 + i4

test2 :: Effect (Either String Int)
test2 = runExceptT program1

program2 :: forall m. MonadError String m => MonadEffect m => m Int
program2 = do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 4) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  i4 <- flip catchError (\e -> if i2 < 5 then pure 8 else throwError e) do
    i5 <- liftEffect $ Random.randomInt 1 10
    when (i5 < 5) do
      throwError "below 5"
    pure i5
  pure $ 1 + i1 + i2 + i3 + i4

test3 :: Effect (Either String Int)
test3 = runExceptT program2
