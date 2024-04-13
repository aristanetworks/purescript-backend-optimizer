-- @inline Data.Generic.Rep.Sum always
-- @inline Data.Show.Generic.genericShowConstructor arity=2
-- @inline export genericTest.from arity=1
module Snapshot.KnownConstructors08 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Test = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

derive instance genericTest :: Generic Test _

instance Show Test where
  show = genericShow
