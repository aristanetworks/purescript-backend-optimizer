module Snapshot.EffectRefs01 where

import Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref

foreign import g :: forall a. a -> a
foreign import r :: Ref Int

test1 = Ref.new 42
test2 = Ref.new (g 42)

test3 = Ref.read r
test4 = Ref.read (g r)

test5 = Ref.write 42 r
test6 = Ref.write (g 42) r

test7 = Ref.modify g r
test8 = Ref.modify (g g) r

test9 = do
  ref <- Ref.new (g 42)
  prev <- Ref.read ref
  Ref.write (prev + 1) ref
  _ <- Ref.modify (add 1) ref
  Ref.read ref
