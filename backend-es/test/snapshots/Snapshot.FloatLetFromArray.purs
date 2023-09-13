module Snapshot.FloatLetFromArray where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

fst :: forall (a :: Type). Array (a) -> a
fst a = unsafePartial $ Array.unsafeIndex a 0

snd :: forall (a :: Type). Array (a) -> a
snd a = unsafePartial $ Array.unsafeIndex a 1

left :: forall (a :: Type) (b :: Type). Either a b -> a
left = unsafePartial <<< case _ of
  Left x -> x

right :: forall b a. Either a b -> b
right = unsafePartial <<< case _ of
  Right x -> x

test1 :: Array (Either Int (Array (Either (Array (Either Int (Array (Either (Effect Unit) (Array (Either Int (Effect Unit))))))) Int)))
test1 =
  [ Left 5, let a = [ Left 1, let b = log "foo" in Right [ Left b, Right [ Left 5, Right b ] ] ] in Right [ Left a, Right (1 + (left $ fst $ right $ snd $ right $ snd a)) ] ]
