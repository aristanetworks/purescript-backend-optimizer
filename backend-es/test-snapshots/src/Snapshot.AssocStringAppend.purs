module Snapshot.AssocStringAppend where

import Prelude

test1 :: String -> String
test1 x = "a" <> ((((("b" <> x) <> x) <> x) <> x) <> "c") <> "d"

test2 :: String -> String
test2 x = "a" <> ("b" <> (x <> (x <> (x <> (x <> "c"))))) <> "d"

test3 :: String -> String
test3 x = "a" <> ("b" <> (x <> (x <> (x <> (x <> "c"))))) <> "d" <> ((((("e" <> x) <> x) <> x) <> x) <> "f") <> "g"
