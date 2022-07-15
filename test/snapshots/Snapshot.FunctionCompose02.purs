module Snapshot.FunctionCompose02 (test1, test2, test3, test4) where

import Prelude

foreign import f :: Int -> Int
foreign import g :: Int -> Int

test1 = f <<< g
test2 = g <<< (f <<< g)
test3 = (f <<< g) <<< (f <<< g)
test4 = ((g <<< f) <<< g) <<< (f <<< g)
