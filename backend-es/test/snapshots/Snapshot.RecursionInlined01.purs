-- @inline Snapshot.RecursionInlined01.append always
module Snapshot.RecursionInlined01 where

data List a = Nil | Cons a (List a)
infixr 5 Cons as :

append :: forall a. List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

infixr 4 append as <>

main :: List String
main = ("a" : "b" : "c" : "d" : "e" : "f" : "g" : Nil) <> Nil