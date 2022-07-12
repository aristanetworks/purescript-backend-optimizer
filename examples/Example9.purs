module Example9 where

import Prelude

import Data.Variant (match, on)
import Heterogeneous.Mapping (hmap)
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