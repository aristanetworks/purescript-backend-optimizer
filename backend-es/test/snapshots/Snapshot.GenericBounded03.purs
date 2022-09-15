module Snapshot.GenericBounded03 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Generic.Rep (class Generic)

data Test3 = Both3 Int Int

derive instance Eq Test3
derive instance Ord Test3
derive instance genericTest3 :: Generic Test3 _

instance Bounded Test3 where
  bottom = genericBottom
  top = genericTop
