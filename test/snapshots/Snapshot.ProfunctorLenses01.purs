-- @inline Data.Lens.Lens.lens arity=2
-- @inline Data.Lens.Record.prop arity=4
-- @inline Data.Profunctor.Strong.strongFn.first arity=1
module Snapshot.ProfunctorLenses01 where

import Prelude

import Data.Lens (over, view)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

test1 = view (prop (Proxy :: _ "foo"))
test2 a = view (prop (Proxy :: _ "foo")) a

test3 = over (prop (Proxy :: _ "bar")) (add 1)
test4 a = over (prop (Proxy :: _ "bar")) (add 1) a

test5 = over (prop (Proxy :: _ "bar") <<< prop (Proxy :: _ "baz")) (add 1)
test6 a = over (prop (Proxy :: _ "bar") <<< prop (Proxy :: _ "baz")) (add 1) a

test7 =
  over (prop (Proxy :: _ "foo")) (add 1)
    >>> over (prop (Proxy :: _ "bar")) (add 42)

test8 a = a
  # over (prop (Proxy :: _ "foo")) (add 1)
  # over (prop (Proxy :: _ "bar")) (add 42)
