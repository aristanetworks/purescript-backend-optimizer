module Example9 where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (match, on)
import Heterogeneous.Mapping (hmap, hmapWithIndex, class MappingWithIndex)
import Prim.Row as Row
import Record as Record
import Record.Builder as Record.Builder
import Type.Proxy (Proxy(..))

test1 r =
  Record.get (Proxy :: _ "foo") r

test1' =
  Record.get (Proxy :: _ "foo")
    { foo: 42
    , bar: "hello"
    }

test2 =
  Record.Builder.buildFromScratch
    ( Record.Builder.insert (Proxy :: _ "foo") 42
        >>> Record.Builder.insert (Proxy :: _ "bar") "hello"
        >>> Record.Builder.insert (Proxy :: _ "baz") true
        >>> Record.Builder.delete (Proxy :: _ "baz")
        >>> Record.Builder.modify (Proxy :: _ "bar") (_ <> ", world!")
        >>> Record.Builder.rename (Proxy :: _ "bar") (Proxy :: _ "qux")
    )

test3 =
  hmap (show :: String -> String)
    { foo: "hello"
    , bar: "world"
    }

test4 =
  on (Proxy :: _ "foo") (\a -> show (a + 1))
    >>> on (Proxy :: _ "bar") (\a -> a <> ", world")
    >>> on (Proxy :: _ "baz") (\a -> show (a - 1))

test5 :: { foo :: Int , bar :: Int } -> { foo :: String, bar :: String }
test5 = hmap (show :: Int -> String)

newtype ZipProps fns = ZipProps { | fns }

instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProps fns) (Proxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns

zipRecord = hmapWithIndex <<< ZipProps

test6 =
  { a: add 1
  , b: Tuple "bar"
  , c: \a -> not a
  }
  `zipRecord`
  { a: 12
  , b: 42.0
  , c: true
  }

test7 :: { a :: Int, b :: Number, c :: Boolean } -> _
test7 = zipRecord
  { a: add 1
  , b: Tuple "bar"
  , c: \a -> not a
  }

test8 = do
  let bar = { bar: "world" }
  let wat = { foo: "hello", bar }
  \_ -> wat.foo <> ", " <> wat.bar.bar