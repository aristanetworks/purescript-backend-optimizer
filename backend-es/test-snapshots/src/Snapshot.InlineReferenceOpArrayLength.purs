-- @inline export fn' never
module Snapshot.InlineReferenceOpArrayLength where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

test1 :: (forall r. r -> Int) -> Array Int
test1 fn = do
  let array = [ 1, 2, fn {} ]
  if Array.length array == 3 then
    array
  else
    []

test2 :: (forall r. r -> Int) -> Array (Array Int)
test2 fn = do
  let array1 = [ 1, 2, fn {} ]
  let array2 = [ array1, [ 3, 4 ], [ fn {} ] ]
  if Just 3 == (Array.length <$> Array.index array2 0) then
    array2
  else
    [ array1 ] <> array2

fn' :: forall r. r -> Int
fn' _ = 0

extern1 :: Array Int
extern1 = [ 1, 2, fn' {} ]

extern2 :: Array (Array Int)
extern2 = [ extern1, [ 3 ], [ fn' {} ] ]

test3 :: Array Int
test3 =
  if Array.length extern1 == 3 then
    extern1
  else
    []

test4 :: Array (Array Int)
test4 =
  if Just 3 == (Array.length <$> Array.index extern2 0) then
    extern2
  else
    []
