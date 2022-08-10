module Snapshot.CaseInt where

test1 :: Int -> String
test1 = case _ of
  1 -> "1"
  2 -> "2"
  3 -> "3"
  _ -> "catch"
