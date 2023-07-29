module Snapshot.InlineLocalReferenceOpIsTag where

data List a = Cons a (List a) | Nil

test1 :: (forall r. r -> List Int) -> List Int
test1 fn = do
  let list = Cons 1 (fn {})
  case list of
    Cons _ _ ->
      Cons 0 list
    _ ->
      Nil

test2 :: (forall r. r -> List Int) -> List Int
test2 fn = do
  let rec = { a: { b: { c: Cons 1 (fn {}) } } }
  case rec.a.b.c of
    list@(Cons _ _) ->
      Cons 0 list
    _ ->
      fn rec

test3 :: (forall r. r -> List Int) -> List Int
test3 fn = do
  let rec1 = { a: { b: { c: Cons 1 (fn {}) } } }
  let rec2 = { d: rec1, e: fn {} }
  case rec2.d.a.b.c of
    list@(Cons _ _) ->
      Cons 0 list
    _ ->
      fn { rec1, rec2 }
