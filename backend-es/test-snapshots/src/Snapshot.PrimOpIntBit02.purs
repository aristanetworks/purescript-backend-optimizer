module Snapshot.PrimOpIntBit02 where

import Data.Int.Bits

import Prelude (negate)

test1 = 1023 .&. 8
test2 = 16 .|. 15
test3 = 1023 `shl` 2
test4 = -1023 `shr` 2
test5 = 15 `xor` 12
test6 = -1023 `zshr` 2
test7 = complement (-3)
