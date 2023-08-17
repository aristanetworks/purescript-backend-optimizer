-- @inline Heterogeneous.Mapping.hmapRecord arity=2
-- @inline Heterogeneous.Mapping.hmapWithIndexRecord arity=2
-- @inline Heterogeneous.Mapping.mapRecordWithIndexCons arity=5
-- @inline Heterogeneous.Mapping.mapRecordWithIndexNil.mapRecordWithIndexBuilder arity=2
module Snapshot.Heterogeneous01
  ( test1
  , test2
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Mapping (hmap, hmapWithIndex, class MappingWithIndex)
import Prim.Row as Row
import Record as Record
import Record.Builder as Record.Builder
import Type.Proxy (Proxy(..))

newtype ZipProps fns = ZipProps { | fns }

instance zipProps ::
  ( IsSymbol sym
  , Row.Cons sym (a -> b) x fns
  ) =>
  MappingWithIndex (ZipProps fns) (Proxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns

zipRecord = hmapWithIndex <<< ZipProps

test1 =
  { a: add 1
  , b: Tuple "bar"
  , c: \a -> not a
  }
    `zipRecord`
      { a: 12
      , b: 42.0
      , c: true
      }

test2 :: { a :: Int, b :: Number, c :: Boolean } -> _
test2 = zipRecord
  { a: add 1
  , b: Tuple "bar"
  , c: \a -> not a
  }
