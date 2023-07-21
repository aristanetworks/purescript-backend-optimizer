module Snapshot.PrimOpString03 where

import Prelude

test1 :: String -> String
test1 a = "a" <> "b" <> a <> "c" <> "d"

test2 :: String -> String
test2 a = "a" <> (("b" <> a) <> "c") <> "d"

test3 :: String -> String
test3 a = "a" <> ("b" <> (a <> "c")) <> "d"
