module Snapshot.CaseNamed where

import Prelude

test1 :: Int -> String
test1 = case _ of
  one@one'@one''@1 -> show one <> show one' <> show one''
  two@2 -> show two
  any0@any1@any -> "any: " <> show any0 <> show any1 <> show any

data Product3 = Product3 Int Int Int

test2 :: Product3 -> String
test2 = case _ of
  Product3 c@1 a b -> show a <> show b <> show c
  Product3 a c@1 b -> show a <> show b <> show c
  Product3 a b c@1 -> show a <> show b <> show c
  Product3 a@x b@y c@z -> show a <> show x <> show b <> show y <> show c <> show z
