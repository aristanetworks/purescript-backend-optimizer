module Snapshot.ShowLiterals where

import Prelude

test1 = show 42 :: String
test2 = show 42.0 :: String
test3 = show true :: String
test4 = show "wat" :: String
test5 = show 'w' :: String
test6 = show { foo: "1", bar: true } :: String
test7 = show [ 1, 2, 3, 4 ] :: String
