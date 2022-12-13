-- @fails Binding demanded before initialized
module Snapshot.RecursiveBindingGroup01 where

import Prelude

test1 :: { bar :: Int, foo :: Int }
test1 =
  { foo: (\_ -> test2.baz) unit
  , bar: (\_ -> test3 42) unit
  }

test2 :: { baz :: Int }
test2 =
  { baz: (\_ -> test1.bar) unit
  }

test3 :: Int -> Int
test3 n
  | n < 100 = n
  | otherwise = test1.bar
