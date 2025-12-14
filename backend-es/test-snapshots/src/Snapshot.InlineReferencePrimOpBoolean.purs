-- @inline export fn never
module Snapshot.InlineReferencePrimOpBoolean where

import Prelude

fn :: forall r. r -> Int
fn _ = 0

test1 :: Int
test1 = do
  let rec = { a: { b: { c: true } }, d: fn {}, e: true, f: false }
  if rec.a.b.c && rec.e then
    42
  else
    fn rec

test2 :: Int
test2 = do
  let rec = { a: { b: { c: true } }, d: fn {}, e: true, f: false }
  if rec.f || rec.a.b.c then
    42
  else
    fn rec

test3 :: Int
test3 = do
  let rec = { a: { b: { c: true } }, d: fn {}, e: true, f: false }
  if not rec.a.b.c then
    fn rec
  else
    42

extern1 :: { a :: { b :: { c :: Boolean } }, d :: Int, e :: Boolean, f :: Boolean }
extern1 = { a: { b: { c: true } }, d: fn {}, e: true, f: false }

test4 :: Int
test4 =
  if extern1.a.b.c && extern1.e then
    42
  else
    99

test5 :: Int
test5 =
  if extern1.f || extern1.a.b.c then
    42
  else
    99

test6 :: Int
test6 =
  if not extern1.a.b.c then
    99
  else
    42
