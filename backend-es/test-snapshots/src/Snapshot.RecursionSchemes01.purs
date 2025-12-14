-- @inline Snapshot.RecursionSchemes01.cata arity=2
-- @inline Snapshot.RecursionSchemes01.functorExprF.map arity=2
module Snapshot.RecursionSchemes01 (Fix, ExprF, test1, test2) where

import Prelude

newtype Fix f = Fix (f (Fix f))

data ExprF a = Add a a | Mul a a | Lit Int

derive instance Functor ExprF

type Expr = Fix ExprF

eval :: ExprF Int -> Int
eval = case _ of
  Add a b -> a + b
  Mul a b -> a * b
  Lit a -> a

bump :: ExprF Int -> ExprF Int
bump = case _ of
  Lit a -> Lit (a + 1)
  other -> other

cata :: forall f a. Functor f => (f a -> a) -> Fix f -> a
cata alg = go
  where
  go (Fix f) = alg (map go f)

test1 :: Expr -> Int
test1 = cata eval

test2 :: Expr -> Int
test2 = cata (eval <<< bump)
