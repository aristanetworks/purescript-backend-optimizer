module Snapshot.GenericBounded02 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Generic.Rep (class Generic)

data Test2 = Bottom2 Int | Ignored2 | Top2 Int

derive instance Eq Test2
derive instance Ord Test2
derive instance genericTest2 :: Generic Test2 _

instance Bounded Test2 where
  bottom = genericBottom
  top = genericTop
