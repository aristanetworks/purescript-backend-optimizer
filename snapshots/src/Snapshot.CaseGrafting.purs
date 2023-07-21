module Snapshot.CaseGrafting where

test1 :: Boolean -> Boolean -> Boolean -> Int
test1 = case _, _, _ of
  _, false, true -> 1
  false, true, _ -> 2
  _, _, false -> 3
  _, _, true -> 4
