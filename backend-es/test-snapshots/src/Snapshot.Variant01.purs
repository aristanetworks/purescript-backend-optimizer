module Snapshot.Variant01 where

import Prelude

import Data.Variant (Variant, case_, inj, on)
import Type.Proxy (Proxy(..))

test1 :: Variant (foo :: Int, bar :: Boolean, baz :: String) -> String
test1 = case_
  # on (Proxy :: _ "foo") (\a -> show a)
  # on (Proxy :: _ "bar") (\a -> show a)
  # on (Proxy :: _ "baz") (\a -> a)

test2 :: forall r. (Variant r -> String) -> Variant (foo :: Int, bar :: Boolean, baz :: String | r) -> String
test2 =
  on (Proxy :: _ "foo") (\a -> show a)
    <<< on (Proxy :: _ "bar") (\a -> show a)
    <<< on (Proxy :: _ "baz") (\a -> a)

test3 :: forall r. Variant (foo :: Int | r)
test3 = inj (Proxy :: _ "foo") 42
