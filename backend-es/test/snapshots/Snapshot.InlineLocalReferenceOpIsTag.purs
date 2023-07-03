module Snapshot.InlineLocalReferenceOpIsTag where

data List a = Cons a (List a) | Nil

test :: ({} -> List Int) -> List Int
test fn = do
  let list = Cons 1 (fn {})
  case list of
    Cons _ _ ->
      Cons 0 list
    _ ->
      Nil
