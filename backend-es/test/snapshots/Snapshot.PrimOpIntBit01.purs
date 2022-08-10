module Snapshot.PrimOpIntBit01 where

import Data.Int.Bits

foreign import a :: Int
foreign import b :: Int

test1 = a .&. b
test2 = a .|. b
test3 = a `shl` b
test4 = a `shr` b
test5 = a `xor` b
test6 = a `zshr` b
test7 = complement a
