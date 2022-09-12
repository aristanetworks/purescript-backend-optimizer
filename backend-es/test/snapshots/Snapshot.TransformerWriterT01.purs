module Snapshot.TransformerWriterT01 where

import Prelude

import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Random as Random

test1 :: Effect Int
test1 = map fst $ runWriterT do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 1) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  tell "nothing"
  pure $ 4 + i1 + i2 + i3

test2 :: forall m. MonadTell String m => MonadEffect m => m Int
test2 = do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 1) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  tell "nothing"
  pure $ 4 + i1 + i2 + i3

test3 :: Effect Int
test3 = map fst $ runWriterT test2

test4 :: forall m. MonadEffect m => m (Tuple Int String)
test4 = runWriterT do
  liftEffect $ Console.log "foo"
  i1 <- liftEffect $ Random.randomInt 1 10
  i2 <- map (_ + 1) $ liftEffect $ Random.randomInt 1 10
  i3 <- (+) <$> (liftEffect $ Random.randomInt 1 10) <*> (liftEffect $ Random.randomInt 1 10)
  tell "nothing"
  pure $ 4 + i1 + i2 + i3

test5 :: Effect Int
test5 = map fst test4
