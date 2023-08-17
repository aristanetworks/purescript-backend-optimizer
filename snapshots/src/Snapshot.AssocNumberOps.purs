module Snapshot.AssocNumberOps where

import Prelude

test1 :: Number -> Number
test1 x = 1.0 + (((((2.0 + x) + x) + x) + x) + 3.0) + 4.0

test2 :: Number -> Number
test2 x = 1.0 + (2.0 + (x + (x + (x + (x + 3.0))))) + 4.0

test3 :: Number -> Number
test3 x = 1.0 + (2.0 + (x + (x + (x + (x + 3.0))))) + 4.0 + (((((5.0 + x) + x) + x) + x) + 6.0) + 7.0

test4 :: Number -> Number
test4 x = 1.0 * (((((2.0 * x) * x) * x) * x) * 3.0) * 4.0

test5 :: Number -> Number
test5 x = 1.0 * (2.0 * (x * (x * (x * (x * 3.0))))) * 4.0

test6 :: Number -> Number
test6 x = 1.0 * (2.0 * (x * (x * (x * (x * 3.0))))) * 4.0 * (((((5.0 * x) * x) * x) * x) * 6.0) * 7.0
