-- @inline export fn never
module Snapshot.InlineReferenceIfThenElse where

fn :: forall r. r -> Int
fn _ = 0

test1 :: Int
test1 = do
  let rec = { a: { b: { c: true } }, d: fn {} }
  if rec.a.b.c then
    42
  else
    fn rec

extern1 :: { a :: { b :: { c :: Boolean } }, d :: Int }
extern1 = { a: { b: { c: true } }, d: fn {} }

test2 :: Int
test2 =
  if extern1.a.b.c then
    42
  else
    99
