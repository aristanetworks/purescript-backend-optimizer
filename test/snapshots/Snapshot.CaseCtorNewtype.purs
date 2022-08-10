module Snapshot.CaseNewtype where

newtype NewTypeInt = NewTypeInt Int

test1 :: NewTypeInt -> String
test1 = case _ of
  NewTypeInt 1 -> "1"
  NewTypeInt 2 -> "2"
  NewTypeInt 3 -> "3"
  _ -> "catch"
