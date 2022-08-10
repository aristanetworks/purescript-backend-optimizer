module Snapshot.CaseChar where

test1 :: Char -> String
test1 = case _ of
  'a' -> "1"
  'b' -> "2"
  'c' -> "3"
  _ -> "catch"
