module Snapshot.FunctionCompose01 where

import Prelude

f _ = "a"
g _ = "b"

test1 = f <<< g
test2 = g <<< (f <<< g)
test3 = (f <<< g) <<< (f <<< g)
test4 = ((g <<< f) <<< g) <<< (f <<< g)
