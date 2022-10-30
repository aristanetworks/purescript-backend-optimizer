module Snapshow.TopLevelHygiene02 where

import Snapshot.TopLevelHygiene01 (test1)

wat :: Int
wat = 42

test2 :: forall a b. (a -> b) -> a -> b
test2 = test1
