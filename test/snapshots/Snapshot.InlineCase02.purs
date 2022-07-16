-- @inline Data.Maybe.maybe arity=3
-- @inline Data.Maybe.maybe' arity=3
module Snapshot.InlineCase02 where

import Prelude

import Data.Maybe (Maybe, maybe, maybe')

foreign import a :: Int
foreign import f :: Unit -> Int
foreign import g :: Int -> Int -> Int

-- Terms are purposefully eta-expanded.

test1 :: Maybe Int -> Int
test1 z = maybe (f unit) (add 1) z

test2 :: Maybe Int -> Int
test2 z = maybe (f unit) (g 1) z

test3 :: Maybe Int -> Int
test3 z = maybe' f (add 1) z

test4 :: Maybe Int -> Int
test4 z = maybe' f (g 1) z

test5 :: Maybe Int -> Int
test5 z = maybe' (\_ -> a + 1) (g 1) z
