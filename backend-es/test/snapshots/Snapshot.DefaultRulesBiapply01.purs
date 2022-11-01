module Snapshot.DefaultRulesBiapply01 where

import Prelude

import Control.Biapply (biapplyFirst, biapplySecond, bilift2, bilift3)
import Data.Tuple (Tuple(..))

test1 = biapplyFirst (Tuple 'b' 2) (Tuple 'a' 1)

test2 = biapplySecond (Tuple 'b' 2) (Tuple 'a' 1)

test3 = bilift2
  (\a b -> a + b)
  (\c d -> c <> d)
  (Tuple 1 "hello")
  (Tuple 2 " world")

test4 = bilift3
  (\a b c -> a + b + c)
  (\d e f -> d <> e <> f)
  (Tuple 1 "hello")
  (Tuple 2 " ")
  (Tuple 3 "world")
