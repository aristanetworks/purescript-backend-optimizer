module Snapshot.FunctionCompose02 (test1, test2, test3, test4) where

import Prelude

type F = Int -> Int

test1 :: F -> F -> F
test1 f g = f <<< g

test2 :: F -> F -> F
test2 f g = g <<< (f <<< g)

test3 :: F -> F -> F
test3 f g = (f <<< g) <<< (f <<< g)

test4 :: F -> F -> F
test4 f g = ((g <<< f) <<< g) <<< (f <<< g)
