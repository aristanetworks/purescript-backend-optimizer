module Snapshot.EffectLoops03 where

import Prelude

import Effect (Effect, whileE)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref

test1 :: Ref Boolean -> Effect Unit
test1 cond = whileE (Ref.read cond) do
  Console.log "foo"
  Console.log "bar"

test2 :: Ref Boolean -> Effect Unit
test2 cond = do
  whileE (Ref.read cond) do
    Console.log "foo"
  whileE (Ref.read cond) do
    Console.log "bar"

test3 :: Ref Boolean -> Ref Int -> Effect Unit
test3 cond ref = do
  whileE (Ref.read cond) do
    a <- Ref.read ref
    when (a < 10) do
      Console.log "foo"

test4 :: Ref Boolean -> Ref Int -> Effect Unit
test4 cond ref = do
  whileE (Ref.read cond) do
    a <- Ref.read ref
    void $
      if a < 10 then
        Console.log "foo"
      else
        Console.log "wat"
