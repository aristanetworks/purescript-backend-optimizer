-- @inline export genericTest1.to arity=1
-- @inline export genericTest1.from arity=1
module Snapshot.GenericEnum01 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)

data Test1
  = A1
  | B1
  | C1
  | D1

derive instance Eq Test1
derive instance Ord Test1
derive instance genericTest1 :: Generic Test1 _

instance Bounded Test1 where
  bottom = genericBottom
  top = genericTop

instance Enum Test1 where
  pred = genericPred
  succ = genericSucc
