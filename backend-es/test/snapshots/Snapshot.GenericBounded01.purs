-- @inline export genericTest1.to arity=1
module Snapshot.GenericBounded01 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Generic.Rep (class Generic)

data Test1 = Bottom1 | Ignored1 | Top1

derive instance Eq Test1
derive instance Ord Test1
derive instance genericTest1 :: Generic Test1 _

instance Bounded Test1 where
  bottom = genericBottom
  top = genericTop
