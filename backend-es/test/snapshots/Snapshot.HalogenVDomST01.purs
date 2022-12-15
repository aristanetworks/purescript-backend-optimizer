module Snapshot.HalogenVDomST01 where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Array as Array
import Data.Array.ST as STArray
import Effect (forE, foreachE)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, mkEffectFn5, mkEffectFn6, runEffectFn2, runEffectFn3, runEffectFn4)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST as STObject
import Foreign.Object.ST.Unsafe as STObject.Unsafe
import Foreign.Object.Unsafe as Object.Unsafe
import Partial.Unsafe as Partial

diffWithIxE
  :: forall b c d
   . EffectFn5
       (Array b)
       (Array c)
       (EffectFn3 Int b c d)
       (EffectFn2 Int b Unit)
       (EffectFn2 Int c d)
       (Array d)
diffWithIxE = mkEffectFn5 \a1 a2 f1 f2 f3 -> do
  a3 <- liftST $ STArray.new
  let l1 = Array.length a1
  let l2 = Array.length a2
  let l3 = if l1 < l2 then l2 else l1
  forE 0 l3 \i ->
    if i < l1 then
      if i < l2 then do
        let v1 = Partial.unsafePartial Array.unsafeIndex a1 i
        let v2 = Partial.unsafePartial Array.unsafeIndex a2 i
        v3 <- runEffectFn3 f1 i v1 v2
        void $ liftST $ STArray.push v3 a3
      else do
        let v1 = Partial.unsafePartial Array.unsafeIndex a1 i
        runEffectFn2 f2 i v1
    else if i < l2 then do
      let v2 = Partial.unsafePartial Array.unsafeIndex a2 i
      v3 <- runEffectFn2 f3 i v2
      void $ liftST $ STArray.push v3 a3
    else
      pure unit
  liftST $ STArray.unsafeFreeze a3

diffWithKeyAndIxE
  :: forall a b c d
   . EffectFn6
       (Object a)
       (Array b)
       (b -> String)
       (EffectFn4 String Int a b c)
       (EffectFn2 String a d)
       (EffectFn3 String Int b c)
       (Object c)
diffWithKeyAndIxE = mkEffectFn6 \o1 as fk f1 f2 f3 -> do
  o2 <- liftST STObject.new
  forE 0 (Array.length as) \i -> do
    let a = Partial.unsafePartial Array.unsafeIndex as i
    let k = fk a
    if Object.member k o1 then do
      let v1 = Object.Unsafe.unsafeIndex o1 k
      v2 <- runEffectFn4 f1 k i v1 a
      void $ liftST $ STObject.poke k v2 o2
    else do
      v2 <- runEffectFn3 f3 k i a
      void $ liftST $ STObject.poke k v2 o2
  o3 <- liftST $ STObject.Unsafe.unsafeFreeze o2
  foreachE (Object.keys o1) \k ->
    if Object.member k o3 then
      pure unit
    else
      void $ runEffectFn2 f2 k (Object.Unsafe.unsafeIndex o1 k)
  pure o3
