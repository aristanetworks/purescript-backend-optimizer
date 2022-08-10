module Snapshot.FunctionCompose03 (test1, test2, test3, test4) where

import Prelude

foreign import f :: Unit -> Int -> Int
foreign import g :: Unit -> Int -> Int

test1 = f unit <<< g unit
test2 = g unit <<< (f unit <<< g unit)
test3 = (f unit <<< g unit) <<< (f unit <<< g unit)
test4 = ((g unit <<< f unit) <<< g unit) <<< (f unit <<< g unit)
