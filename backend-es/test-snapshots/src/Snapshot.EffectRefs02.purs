module Snapshot.EffectRefs02 where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect, whileE)
import Effect.Ref (Ref)
import Effect.Ref as Ref

test1 :: Int -> Effect Int
test1 hi = do
  count <- Ref.new 0
  continue <- Ref.new true
  whileE (Ref.read continue) do
    n <- Ref.read count
    void $
      if n < hi then
        Ref.write (n + 1) count
      else
        Ref.write false continue
  Ref.read count

test2 :: Effect (Int -> Effect Unit)
test2 = do
  count <- Ref.new 0
  pure \n -> void $ Ref.modify (_ + n) count

test3 :: Effect (Tuple (Ref Int) (Int -> Effect Unit))
test3 = do
  count <- Ref.new 0
  pure $ Tuple count \n -> void $ Ref.modify (_ + n) count
