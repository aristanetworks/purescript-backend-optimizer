module Snapshot.CaseHeuristics where

data Column
  = Zero
  | One Int
  | Two Int Int

testP :: Int -> Int -> Int -> Int
testP = case _, _, _ of
  1, 2, 1 -> 1
  1, 2, 2 -> 2
  _, 2, 3 -> 3
  1, _, 4 -> 4
  _, _, _ -> 5

testPB :: Column -> Column -> Int
testPB = case _, _ of
  One 1, One 1 -> 1
  Two 2 3, Two 2 3 -> 2
  _, Zero -> 3
  _, _ -> 4

testPBA :: Column -> Column -> Int
testPBA = case _, _ of
  One 1, One 1 -> 1
  One 2, One 2 -> 2
  Two 1 _, Two _ _ -> 3
  _, _ -> 4

testPBAN :: Column -> Column -> Int
testPBAN = case _, _ of
  One 1, One 1 -> 1
  One 2, One 2 -> 2
  Two _ _, Two _ _ -> 3
  _, _ -> 4
