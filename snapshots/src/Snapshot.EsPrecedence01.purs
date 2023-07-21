module Snapshot.EsPrecedence01 where

import Prelude

test1 :: (Unit -> Boolean) -> Unit -> Unit -> Boolean
test1 f a b = do
  let x = if f a then f b else false
  let y = if x then f a else true
  if y then f a else f unit
