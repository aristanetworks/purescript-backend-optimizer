-- @inline Data.Maybe.maybe arity=2
-- @inline Data.Maybe.maybe' arity=2
module Snapshot.InlineCase01 where

import Prelude

import Data.Maybe (Maybe, maybe, maybe')

-- Terms are purposefully eta-reduced

test1 :: (Unit -> Int) -> Maybe Int -> Int
test1 f = maybe (f unit) (add 1)

test2 :: forall a b. (Unit -> b) -> (Int -> a -> b) -> Maybe a -> b
test2 f g = maybe (f unit) (g 1)

test3 :: (Unit -> Int) -> Maybe Int -> Int
test3 f = maybe' f (add 1)

test4 :: forall a b. (Unit -> b) -> (Int -> a -> b) -> Maybe a -> b
test4 f g = maybe' f (g 1)

test5 :: forall a. Int -> (Int -> a -> Int) -> Maybe a -> Int
test5 a g = maybe' (\_ -> a + 1) (g 1)
