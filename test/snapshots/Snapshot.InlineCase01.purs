-- @inline Data.Maybe.maybe arity=2
-- @inline Data.Maybe.maybe' arity=2
module Snapshot.InlineCase01 where

import Prelude

import Data.Maybe (Maybe, maybe, maybe')

foreign import a :: Int
foreign import f :: Unit -> Int
foreign import g :: Int -> Int -> Int

-- Terms are purposefully eta-reduced

test1 :: Maybe Int -> Int
test1 = maybe (f unit) (add 1)

test2 :: Maybe Int -> Int
test2 = maybe (f unit) (g 1)

test3 :: Maybe Int -> Int
test3 = maybe' f (add 1)

test4 :: Maybe Int -> Int
test4 = maybe' f (g 1)

test5 :: Maybe Int -> Int
test5 = maybe' (\_ -> a + 1) (g 1)
