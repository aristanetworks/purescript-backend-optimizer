-- @inline export fn never
-- @inline export localTest always
-- @inline export externTest always
module Snapshot.InlineReferencePrimOpInt where

import Prelude

fn :: forall r. r -> Int
fn _ = 0

type Rec = { a :: { b :: { c :: Int } }, d :: Int, e :: Int }

localTest :: (Rec -> Int) -> Int
localTest f = do
  let rec = { a: { b: { c: 99 }}, d: fn {}, e: 11 }
  let res = f rec
  if res /= bottom then res
  else fn rec

test1 :: Int
test1 = localTest \rec -> rec.a.b.c + rec.e

test2 :: Int
test2 = localTest \rec -> rec.a.b.c - rec.e

test3 :: Int
test3 = localTest \rec -> rec.a.b.c * rec.e

test4 :: Int
test4 = localTest \rec -> rec.a.b.c / rec.e

extern :: Rec
extern = { a: { b: { c: 99 }}, d: fn {}, e: 11 }

externTest :: (Rec -> Int) -> Int
externTest f = do
  let res = f extern
  if res /= bottom then res
  else bottom

test5 :: Int
test5 = externTest \rec -> rec.a.b.c + rec.e

test6 :: Int
test6 = externTest \rec -> rec.a.b.c - rec.e

test7 :: Int
test7 = externTest \rec -> rec.a.b.c * rec.e

test8 :: Int
test8 = externTest \rec -> rec.a.b.c / rec.e
