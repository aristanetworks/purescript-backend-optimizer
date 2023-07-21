module Snapshot.STObject01 where

import Prelude

import Control.Monad.ST as ST
import Foreign.Object (Object, freezeST, runST)
import Foreign.Object.ST as STObject

test1 :: Object Int
test1 = runST STObject.new

test2 :: Object Int
test2 = ST.run do
  obj <- STObject.new
  freezeST obj

test3 :: Object Int
test3 = runST do
  obj <- STObject.new
  _ <- STObject.poke "a" 1 obj
  _ <- STObject.poke "b" 2 obj
  _ <- STObject.poke "c" 3 obj
  pure obj

test4 :: Object Int
test4 = runST do
  STObject.new
    >>= STObject.poke "a" 1
    >>= STObject.poke "b" 2
    >>= STObject.poke "c" 3

test5 :: Object Int
test5 = runST do
  obj <- STObject.new
  _ <- STObject.poke "a" 1 obj
  _ <- STObject.poke "b" 2 obj
  _ <- STObject.delete "a" obj
  _ <- STObject.delete "b" obj
  pure obj

test6 :: Object Int
test6 = runST do
  STObject.new
    >>= STObject.poke "a" 1
    >>= STObject.poke "b" 2
    >>= STObject.delete "a"
    >>= STObject.delete "b"
