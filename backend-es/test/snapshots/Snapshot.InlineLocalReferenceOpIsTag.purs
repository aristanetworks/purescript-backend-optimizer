module Snapshot.InlineLocalReferenceOpIsTag where

data List a = Cons a (List a) | Nil

test1 :: (forall r. { | r } -> List Int) -> List Int
test1 fn = do
  let list = Cons 1 (fn {})
  case list of
    Cons _ _ ->
      Cons 0 list
    _ ->
      Nil

test2 :: (forall r. { | r } -> List Int) -> List Int
test2 fn = do
  let rec = { a: { b: { c: Cons 1 (fn {}) } } }
  case rec.a.b.c of
    list@(Cons _ _) ->
      Cons 0 list
    _ ->
      fn rec
