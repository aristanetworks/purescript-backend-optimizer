-- @inline export genericTest4.to arity=1
module Snapshot.GenericBounded04 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord1)
import Snapshot.GenericBounded01 (Test1)
import Snapshot.GenericBounded02 (Test2)
import Snapshot.GenericBounded03 (Test3)

data Test4 f a
  = Bottom4 Int Test1 Test2 Test3 a (f a)
  | Ignored4
  | Top4 Int Test1 Test2 Test3 a (f a)

derive instance (Eq1 f, Eq a) => Eq (Test4 f a)
derive instance (Ord1 f, Ord a) => Ord (Test4 f a)
derive instance genericTest4 :: Generic (Test4 f a) _

instance (Ord1 f, Bounded (f a), Bounded a) => Bounded (Test4 f a) where
  bottom = genericBottom
  top = genericTop
