-- @inline Snapshot.RecursionInlined02.append always
module Snapshot.RecursionInlined02 where

data List a = Nil | Cons a (List a)
infixr 5 Cons as :

append :: forall a. List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

infixr 4 append as <>

test1 :: List String -> List String
test1 z = ("a" : "b" : "c" : "d" : "e" : "f" : "g" : z) <> Nil