module Snapshot.CaseSum where

data SumType
  = L Int
  | R Int

test1 :: SumType -> String
test1 = case _ of
  L 1 -> "1"
  L 2 -> "2"
  L _ -> "3"
  R _ -> "4"
