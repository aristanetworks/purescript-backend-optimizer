-- @inline Snapshot.RecursionInlined03.append always
-- This doesn't quite work yet, because the inlining of append depends
-- on the analysis of the local `b`, which we don't have (easy) access to.
-- If we can somehow grab its usage, we'd likely see that access + case == total
module Snapshot.RecursionInlined03 where

import Prelude

import Data.Variant (Variant, inj, case_, on)
import Type.Proxy (Proxy(..))

newtype List a = List (Variant (nil :: Unit, cons :: { head :: a, tail :: List a }))

cons :: forall a. a -> List a -> List a
cons head tail = List $ inj (Proxy :: _ "cons") {head, tail}
nil :: forall a. List a
nil = List $ inj (Proxy :: _ "nil") unit
infixr 5 cons as :

append :: forall a. List a -> List a -> List a
append (List a) b = (case_
  # on (Proxy :: _ "nil") (\_ -> b)
  # on (Proxy :: _ "cons") (\{head, tail} -> cons head (append tail b))) a

infixr 4 append as <>

test1 :: List String
test1 = ("a" : "b" : "c" : nil) <> ("d" : "e" : "f" : "g" : nil)

test2 :: List String -> List String
test2 z = ("a" : "b" : "c" : z) <> ("d" : "e" : "f" : "g" : nil)