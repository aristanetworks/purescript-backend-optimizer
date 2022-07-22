-- @inline Snapshot.Cps02.mkState always
-- @inline Snapshot.Cps02.unState always
-- @inline Snapshot.Cps02.put arity=1
-- @inline Snapshot.Cps02.get always
module Snapshot.Cps02 where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Tuple (Tuple(..))

newtype State s a = State (forall r. Fn2 (Fn2 s a r) s r)

mkState :: forall s a. (forall r. (s -> a -> r) -> s -> r) -> State s a
mkState k = State (mkFn2 \k' s -> k (runFn2 k') s)

unState :: forall s a. State s a -> (forall r. (s -> a -> r) -> s -> r)
unState (State k) k' s = runFn2 k (mkFn2 k') s

instance Functor (State s) where
  map f k = mkState \next1 s1 ->
    unState k (\s2 -> next1 s2 <<< f) s1

instance Apply (State s) where
  apply = ap

instance Applicative (State s) where
  pure a = mkState \next s1 -> next s1 a

instance Bind (State s) where
  bind k1 k2 = mkState \next1 s1 ->
    unState k1 (\s2 a -> unState (k2 a) next1 s2) s1

instance Monad (State s)

get :: forall s. State s s
get = mkState \next s -> next s s

put :: forall s. s -> State s Unit
put s = mkState \next _ -> next s unit

runState :: forall s a. s -> State s a -> Tuple s a
runState s k = unState k Tuple s

test4 :: State Int Unit
test4 = do
  res1 <- get
  put (res1 + 1)
  res2 <- get
  put (res2 + 1)
