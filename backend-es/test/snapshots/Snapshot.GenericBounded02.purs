-- @inline export genericTest2.to arity=1
module Snapshot.GenericBounded02 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Generic.Rep (class Generic)

data Test2
  = Bottom2 Int Boolean { a :: Int, b :: Boolean }
  | Ignored2 Int Boolean { a :: Int, b :: Boolean }
  | Top2 Int Boolean { a :: Int, b :: Boolean }

derive instance Eq Test2
derive instance Ord Test2
derive instance genericTest2 :: Generic Test2 _

instance Bounded Test2 where
  bottom = genericBottom
  top = genericTop
