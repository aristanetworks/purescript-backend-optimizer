module Snapshot.UnsafePerformEffect where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

test :: _ -> Effect Unit
test f = do
  let ref = unsafePerformEffect (Ref.new 0)
  let wat = f ref
  Ref.modify_ (add 1) wat
  Ref.modify_ (add 1) ref
