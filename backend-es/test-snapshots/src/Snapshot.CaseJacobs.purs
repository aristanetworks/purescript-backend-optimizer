module Snapshot.CaseJacobs where

import Prelude

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Succ Expr
  | Zero

instance Show Expr where
  show = case _ of
    Add a b -> "Add(" <> show a <> " " <> show b <> ")"
    Mul a b -> "Mul(" <> show a <> " " <> show b <> ")"
    Succ a -> "Succ(" <> show a <> ")"
    Zero -> "Zero"

test1 :: Expr -> String
test1 = case _ of
  Add Zero Zero -> "e1"
  Mul Zero x -> "e2: " <> show x
  Add (Succ x) y -> "e3: " <> show x <> " " <> show y
  Mul x Zero -> "e4: " <> show x
  Mul (Add x y) z -> "e5: " <> show x <> " " <> show y <> " " <> show z
  Add x Zero -> "e6: " <> show x
  x -> "e7: " <> show x
