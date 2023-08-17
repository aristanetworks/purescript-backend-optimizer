-- @inline Snapshot.BranchSpecialization01.eqTest.eq arity=2
module Snapshot.BranchSpecialization01 where

import Prelude

data Test = Foo | Bar | Baz | Qux

derive instance eqTest :: Eq Test

test1 a = Baz == a

test2 a = a == Baz
