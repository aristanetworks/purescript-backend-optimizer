module Snapshot.RecursiveBindingGroup02 where

import Prelude

test = test1.bar
  where
  test1 =
    { foo: (\_ -> test2.baz) unit
    , bar: (\_ -> test3 42) unit
    }

  test2 =
    { baz: (\_ -> test1.bar) unit
    }

  test3 n
    | n < 100 = n
    | otherwise = test1.bar
