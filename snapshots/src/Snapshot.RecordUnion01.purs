module Snapshot.RecordUnion01 where

import Record (union)

test :: forall r. { | r } -> { foo :: Int | r }
test a = union { foo: 42 } a
