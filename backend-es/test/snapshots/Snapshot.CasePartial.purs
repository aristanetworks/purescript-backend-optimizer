module Snapshot.CasePartial where

test1 :: Partial => Int -> Int
test1 = case _ of
  1 -> 1
  2 -> 2
  3 -> 3
