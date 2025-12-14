module Snapshot.FunctionCompose03 (test1, test2, test3, test4) where

import Prelude

type F = Unit -> Int -> Int

test1 :: F -> F -> Int -> Int
test1 f g = f unit <<< g unit

test2 :: F -> F -> Int -> Int
test2 f g = g unit <<< (f unit <<< g unit)

test3 :: F -> F -> Int -> Int
test3 f g = (f unit <<< g unit) <<< (f unit <<< g unit)

test4 :: F -> F -> Int -> Int
test4 f g = ((g unit <<< f unit) <<< g unit) <<< (f unit <<< g unit)
