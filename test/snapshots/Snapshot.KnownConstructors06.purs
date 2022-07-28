-- @inline Data.Show.Generic.genericShowConstructor arity=2
-- @inline export genericTest.from arity=1
module Snapshot.KnownConstructors06 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Test = Foo | Bar | Baz | Qux

derive instance genericTest :: Generic Test _

instance Show Test where
  show = genericShow
