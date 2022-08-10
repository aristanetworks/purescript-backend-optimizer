module Snapshot.CaseMulti where

test1 :: Int -> Int -> String
test1 = case _, _ of
  1, 1 -> "1.1"
  1, 2 -> "1.2"
  1, 3 -> "1.3"
  _, 4 -> "_.4"
  1, 5 -> "1.5"
  _, 2 -> "_.2"
  _, _ -> "_._"
