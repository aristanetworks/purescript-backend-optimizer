-- @inline export fn' never
module Snapshot.InlineReferenceRecordUpdate where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

test1 :: (forall r. r -> { a :: Int, b :: Int, c :: Int }) -> Int
test1 fn = do
  let rec1 = fn {}
  let rec2 = rec1 { a = 42, b = fn {} }
  if rec2.a == 42 then
    rec2.c
  else
    (fn { rec1, rec2 }).c

-- unsafeCoerce is needed to construct the polymorphic tail.
-- This is only needed to subvert the compiler's record update
-- optimization, which is slated to be removed.
fn' :: forall r x. r -> { a :: Int, b :: Int, c :: Int | x }
fn' _ = unsafeCoerce { a: 1, b: 2, c: 3 }

extern :: forall x. { a :: Int, b :: Int, c :: Int | x }
extern = (fn' {}) { a = 42 }

test2 :: Int
test2 =
  if extern.a == 42 then
    extern.c
  else
    0
