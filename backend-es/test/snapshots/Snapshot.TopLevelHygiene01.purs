-- @inline export test1 always
module Snapshot.TopLevelHygiene01 where

-- This is imported by TopLevelHygiene02, which has a top-level wat binding.
test1 :: forall a b. (a -> b) -> a -> b
test1 = \wat a -> wat a
