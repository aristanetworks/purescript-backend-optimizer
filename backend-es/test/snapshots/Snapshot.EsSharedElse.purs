module Snapshot.EsSharedElse where

test1 :: Boolean -> Boolean -> Boolean -> Int
test1 a b c =
  if a then
    if b then
      1
    else if c then
      2
    else
      3
  else if c then
    2
  else
    3
