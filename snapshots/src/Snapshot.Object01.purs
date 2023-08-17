module Snapshot.Object01 where

import Foreign.Object (Object, keys, member)
import Foreign.Object.Unsafe (unsafeIndex)

test1 :: Object Int -> Int
test1 a = unsafeIndex a "foo"

test2 :: Object Int -> Int
test2 a = unsafeIndex a "foo.bar"

test3 :: Object Int -> String -> Int
test3 a b = unsafeIndex a b

test4 :: Object Int -> Array String
test4 obj = keys obj

test5 :: Object Int -> Boolean
test5 obj = member "wat" obj
