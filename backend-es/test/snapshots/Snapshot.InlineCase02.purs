-- @inline Data.Maybe.maybe arity=3
-- @inline Data.Maybe.maybe' arity=3
module Snapshot.InlineCase02 where

import Prelude

import Data.Maybe (Maybe, maybe, maybe')

-- Terms are purposefully eta-expanded.

test1 :: (Unit -> Int) -> Maybe Int -> Int
test1 f z = maybe (f unit) (add 1) z

test2 :: forall a b. (Unit -> b) -> (Int -> a -> b) -> Maybe a -> b
test2 f g z = maybe (f unit) (g 1) z

test3 :: (Unit -> Int) -> Maybe Int -> Int
test3 f z = maybe' f (add 1) z

test4 :: forall a b. (Unit -> b) -> (Int -> a -> b) -> Maybe a -> b
test4 f g z = maybe' f (g 1) z

test5 :: forall a. Int -> (Int -> a -> Int) -> Maybe a -> Int
test5 a g z = maybe' (\_ -> a + 1) (g 1) z
