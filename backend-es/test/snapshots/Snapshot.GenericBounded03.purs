-- @inline export genericTest3.to arity=1
module Snapshot.GenericBounded03 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Generic.Rep (class Generic)
import Snapshot.GenericBounded01 (Test1)
import Snapshot.GenericBounded02 (Test2)

data Test3 = Both3 Int Boolean Test1 Test2 { a :: Int, b :: Boolean, c :: Test1, d :: Test2 }

derive instance Eq Test3
derive instance Ord Test3
derive instance genericTest3 :: Generic Test3 _

instance Bounded Test3 where
  bottom = genericBottom
  top = genericTop
