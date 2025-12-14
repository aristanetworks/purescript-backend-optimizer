module Snapshot.CaseString where

test1 :: String -> String
test1 = case _ of
  "foo" -> "1"
  "bar" -> "2"
  "" -> "3"
  _ -> "catch"
