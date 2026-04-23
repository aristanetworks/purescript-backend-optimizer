module Snapshot.CaseArray where

test1 :: Array Int -> String
test1 = case _ of
  [] -> "0"
  [ 1 ] -> "1"
  [ _ ] -> "1"
  [ _, 2 ] -> "2"
  [ _, _, _ ] -> "3"
  _ -> "catch"
