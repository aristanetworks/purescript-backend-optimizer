module Snapshot.CaseNumber where

test1 :: Number -> String
test1 = case _ of
  1.0 -> "1"
  2.0 -> "2"
  3.0 -> "3"
  _ -> "catch"
