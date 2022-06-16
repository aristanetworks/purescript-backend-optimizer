module Example5 where

import Prelude

import Data.Function.Uncurried (Fn0, Fn2, Fn7, mkFn0, mkFn2, mkFn7, runFn0, runFn2, runFn7)
import Effect (Effect)
import Effect.Class.Console as Effect.Class.Console
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, mkEffectFn1, mkEffectFn2, mkEffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)

foreign import rand :: Effect Int

test1 :: Fn2 Int Int Int
test1 = mkFn2 const

test2 :: Fn2 Int Int Int
test2 = mkFn2 (\a b -> a)

foreign import fn2 :: Fn2 Int Int Int

test3 :: Int -> Int -> Int
test3 a b = runFn2 fn2 a b

test4 :: Int
test4 = runFn2 test1 10 100

test0' :: Fn0 Int
test0' = mkFn0 \_ -> 10

test0 :: Int
test0 = runFn0 test0'

test7 :: Fn7 Int Int Int Int Int Int Int Int
test7 = mkFn7 \_ _ _ _ _ _ a -> a

test8 :: Int
test8 = runFn7 test7 1 2 3 4 5 6 7

fn :: Unit -> Int
fn = case _ of
  unit -> 10

test9 :: Fn0 Int
test9 = mkFn0 (const 10)

test10 :: Int
test10 = runFn0 test9

effectFn4 :: EffectFn4 Int Int Int Int Int
effectFn4 = mkEffectFn4 (\a b c d -> pure $ const a b)

effect :: Effect Int
effect = runEffectFn4 effectFn4 10 20 30 40

foreign import effectFn2 :: EffectFn2 Int Int Int

test11 :: Int -> Int -> Effect Int
test11 a b = do
  Effect.Class.Console.log "Hello"
  i <- effect
  x <- runEffectFn2 effectFn2 10 10
  let ans = a + b + i + x
  pure ans

test12 :: EffectFn2 Int Int Int
test12 = mkEffectFn2 test11

test13 :: Effect Int
test13 = runEffectFn2 test12 12 20

test14 :: Effect Int
test14 = do
  x <- runEffectFn2 effectFn2 10 10
  runEffectFn2 effectFn2 10 x

test15 :: Effect Int
test15 = do
  n <- do
    x <- rand
    y <- rand
    pure $ x + y
  m <- rand
  pure (n - m)