module Example4 where

import Prelude

import Data.Function.Uncurried (mkFn2, Fn2)
import Data.HeytingAlgebra as Data.HeytingAlgebra
import Effect (Effect)
import Effect.Class.Console as Effect.Class.Console

-- test1 = 10 + 32
-- test2 x = add 1 <<< sub 10 $ x

data Foo = Foo | Bar | Baz

derive instance Eq Foo

test3 = eq Bar

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

data Tuple a b = Tuple a b

runState :: forall s a. s -> State s a -> Tuple s a
runState s (State k) = k Tuple s

test4 = do
 res1 <- get
 put (res1 + 1)
 res2 <- get
 put (res2 + 1)

foreign import boolValue :: Boolean

test6 = false && boolValue

test7 = (false || true) && (10 == 12)

test8 = test6 `Data.HeytingAlgebra.implies` test7

test9 :: Effect Unit
test9 = do
  unless test8 do
    Effect.Class.Console.log "test8 true"
  when ("hello" == "hello") do
    Effect.Class.Console.log "true"

test10 = boolValue || (boolValue && test7)

test11 = case _ of
  true -> false
  false -> true

test12 a =
  if test11 a then false else true

test13 a = if a then true else false

test15 a = case compare a 15 of
  LT -> true
  _ -> false

test16 a = if not a then 12 else 15

test17 (Tuple a b) = a + b

main :: Effect Unit
main = do
  Effect.Class.Console.log "Hello"
  Effect.Class.Console.log "World"
  pure unit
