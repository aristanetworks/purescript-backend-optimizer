module Snapshot.Cps01 where

import Prelude

import Data.Tuple (Tuple(..))

newtype State s a = State (forall r. (s -> a -> r) -> s -> r)

instance Functor (State s) where
  map f (State k) = State \next1 s1 ->
    k (\s2 -> next1 s2 <<< f) s1

instance Apply (State s) where
  apply = ap

instance Applicative (State s) where
  pure a = State \next s1 -> next s1 a

instance Bind (State s) where
  bind (State k1) k2 = State \next1 s1 ->
    k1
      ( \s2 a -> do
          let (State k3) = k2 a
          k3 next1 s2
      )
      s1

instance Monad (State s)

get :: forall s. State s s
get = State \next s -> next s s

put :: forall s. s -> State s Unit
put s = State \next _ -> next s unit

runState :: forall s a. s -> State s a -> Tuple s a
runState s (State k) = k Tuple s

test4 :: State Int Unit
test4 = do
  res1 <- get
  put (res1 + 1)
  res2 <- get
  put (res2 + 1)
