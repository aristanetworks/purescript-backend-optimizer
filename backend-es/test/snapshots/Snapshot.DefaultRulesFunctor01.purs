module Snapshot.DefaultRulesFunctor01 where

import Prelude

import Data.Maybe (Maybe)

test1 :: Maybe Int -> Maybe String
test1 mb = mb <#> \(i :: Int) -> show i

test2 :: forall a. Maybe a -> Maybe Unit
test2 mb = void mb

test3 :: forall a. Maybe a -> Maybe Int
test3 mb = mb $> 42

test4 :: forall a. Maybe a -> Maybe Int
test4 mb = 42 <$ mb

test5 :: forall a. Maybe a -> Maybe a
test5 mb = const <$> mb <@> 12
