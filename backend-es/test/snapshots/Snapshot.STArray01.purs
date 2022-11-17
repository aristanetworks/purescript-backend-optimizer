module Snapshot.STArray01 where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array.ST as Array
import Data.Array.ST as STArray

test1 :: Array Int
test1 = Array.run STArray.new

test2 :: Array Int
test2 = Array.run do
  arr <- STArray.new
  n <- STArray.push 1 arr
  _ <- STArray.pushAll [ 2, n ] arr
  pure arr

test3 :: Array Int
test3 = Array.run do
  arr <- STArray.new
  ST.foreach [1, 2, 3] \a ->
    when (a < 10) do
      void $ STArray.push a arr
  pure arr

test4 :: Array Int
test4 = Array.run do
  arr <- STArray.new
  ST.for 1 1000 \a ->
    when (a == (a / 2) * 2) do
      void $ STArray.push a arr
  pure arr

test5 :: Array Int
test5 = Array.run do
  arr <- STArray.new
  break <- STRef.new true
  ST.while (STRef.read break) do
    void $ STArray.push 1 arr
    STRef.write false break
  pure arr

test6 :: Unit
test6 = ST.run do
  arr <- STArray.new
  break <- STRef.new true
  ST.while (STRef.read break) do
    void $ STArray.push 1 arr
    STRef.write false break

test7 :: Boolean -> Unit
test7 bool = ST.run do
  arr <- STArray.new
  break <- STRef.new true
  if bool then
    ST.while (STRef.read break) do
      void $ STArray.push 1 arr
      STRef.write false break
  else
    pure unit

test8 :: Array Int
test8 = Array.run do
  arr <- STArray.new
  ST.foreach [1, 2, 3] \a ->
    if (a < 10) then
      void $ STArray.push a arr
    else
      void $ STArray.push 12 arr
  pure arr
