module Example2 where

import Control.Semigroupoid

f _ = "a"
g _ = "b"
h = (f <<< g <<< (f <<< g)) <<< g

main = h "c"