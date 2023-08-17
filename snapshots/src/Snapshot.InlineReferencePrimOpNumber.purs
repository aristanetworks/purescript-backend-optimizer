-- @inline export fn never
-- @inline export localTest always
-- @inline export externTest always
module Snapshot.InlineReferencePrimOpNumber where

import Prelude

fn :: forall r. r -> Number
fn _ = 0.0

type Rec = { a :: { b :: { c :: Number } }, d :: Number, e :: Number }

localTest :: (Rec -> Number) -> Number
localTest f = do
  let rec = { a: { b: { c: 99.0 }}, d: fn {}, e: 11.0 }
  let res = f rec
  if res /= bottom then res
  else fn rec

test1 :: Number
test1 = localTest \rec -> rec.a.b.c + rec.e

test2 :: Number
test2 = localTest \rec -> rec.a.b.c - rec.e

test3 :: Number
test3 = localTest \rec -> rec.a.b.c * rec.e

test4 :: Number
test4 = localTest \rec -> rec.a.b.c / rec.e

extern :: Rec
extern = { a: { b: { c: 99.0 }}, d: fn {}, e: 11.0 }

externTest :: (Rec -> Number) -> Number
externTest f = do
  let res = f extern
  if res /= bottom then res
  else bottom

test5 :: Number
test5 = externTest \rec -> rec.a.b.c + rec.e

test6 :: Number
test6 = externTest \rec -> rec.a.b.c - rec.e

test7 :: Number
test7 = externTest \rec -> rec.a.b.c * rec.e

test8 :: Number
test8 = externTest \rec -> rec.a.b.c / rec.e
