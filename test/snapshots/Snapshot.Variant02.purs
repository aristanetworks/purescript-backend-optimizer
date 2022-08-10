-- @inline export variantBuildMatchCons arity=5
module Snapshot.Variant02 where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, case_, on)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class VariantBuildMatch (rl :: RowList Type) r1 r2 r3 z where
  variantBuildMatch :: Proxy rl -> (Variant r1 -> z) -> r2 -> Variant r3 -> z

instance variantBuildMatchCons ::
  ( TypeEquals f (a -> z)
  , Row.Cons sym (a -> z) r2' r2
  , Row.Cons sym a r3' r3
  , IsSymbol sym
  , VariantBuildMatch rl r1 { | r2 } r3' z
  ) =>
  VariantBuildMatch (RowList.Cons sym f rl) r1 { | r2 } r3 z where
  variantBuildMatch _ k r =
    on (Proxy :: _ sym) (Record.get (Proxy :: _ sym) r)
      (variantBuildMatch (Proxy :: _ rl) k r)

instance variantBuildMatchNil :: VariantBuildMatch RowList.Nil r1 r2 r1 z where
  variantBuildMatch _ k _ = k

match
  :: forall r1 r2 rl z
   . RowToList r1 rl
  => VariantBuildMatch rl () { | r1 } r2 z
  => { | r1 }
  -> Variant r2
  -> z
match = variantBuildMatch (Proxy :: _ rl) case_

test1 :: Variant (foo :: Int, bar :: Boolean, baz :: String) -> String
test1 = match
  { foo: \a -> show a
  , bar: \a -> show a
  , baz: \a -> a
  }
