-- @inline export watUnit(..).wat arity=1
module Snapshot.InlineDirectivePropSpine01 where

import Prelude

import Data.Tuple (Tuple)
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEquals

foreign import testImpl :: forall a b. Unit -> Unit

class Wat a b | b -> a where
  wat :: a -> b

instance watUnit :: TypeEquals a Unit => Wat a Unit where
  wat = testImpl <<< TypeEquals.to

-- This results in a partially applied call for `wat` specialized
-- to `Unit` TypeEquals dictionary. Without the directive, This
-- won't inline via heuristics since it's a CAF. With the directive,
-- test1 and test2 get inlined to calls to testImpl.
f = wat :: Unit -> Unit
g = wat :: Unit -> Unit

test1 = f unit :: Unit
test2 = g unit :: Unit
