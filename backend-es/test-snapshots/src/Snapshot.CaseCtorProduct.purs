module Snapshot.CaseProduct where

data Product3 a b c = Product3 a b c

test1 :: Product3 Int Int Int -> String
test1 = case _ of
  Product3 1 2 3 -> "1"
  Product3 _ 4 _ -> "2"
  Product3 4 5 6 -> "3"
  _ -> "catch"
