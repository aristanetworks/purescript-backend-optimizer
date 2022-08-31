-- @inline export  watUnit(..).wat1 arity=1
-- @inline export  watUnit(..).wat2 arity=1
module Snapshot.InlineDirectivePropSpine02 where

import Prelude

import Data.Tuple (Tuple)
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEquals

foreign import testImpl :: forall a b. Unit -> Unit

class Wat a b | b -> a where
  wat1 :: a -> b
  wat2 :: a -> b

instance watUnit :: TypeEquals a Unit => Wat a Unit where
  wat1 = testImpl <<< TypeEquals.to
  wat2 = testImpl <<< TypeEquals.to

-- This results in a partially applied call for `watUnit` specialized
-- to `Unit` TypeEquals dictionary. Without the directive, This
-- won't inline via heuristics since it's a CAF. With the directive,
-- test1 and test2 get inlined to calls to testImpl.
f = wat1 :: Unit -> Unit
g = wat2 :: Unit -> Unit

test1 = f unit :: Unit
test2 = g unit :: Unit
