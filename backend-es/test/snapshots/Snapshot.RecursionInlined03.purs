-- @inline Snapshot.RecursionInlined03.compose always
module Snapshot.RecursionInlined03 where

compose :: forall a b c. (a -> b) -> (b -> c) -> (a -> c)
compose ab bc a = bc (ab a)

id :: forall a. a -> a
id a = a

test1 :: Int -> Int
test1 =compose id (compose id (compose id (compose id id)))
test2 :: (Int -> Int) -> Int -> Int
test2 z = compose id (compose id (compose z (compose id id)))