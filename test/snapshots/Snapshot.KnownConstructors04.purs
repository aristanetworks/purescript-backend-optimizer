module Snapshot.KnownConstructors04 where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

test1 :: Int -> Array String
test1 x = do
  let a = if x > 42 then Just "Hello" else Nothing
  [ unsafePartial fromJust a <> ", World"
  , unsafePartial fromJust a <> ", Universe"
  ]

foreign import f :: String -> String -> String

test2 :: Int -> String
test2 x = do
  let a = if x > 42 then Just "Hello" else Nothing
  f (unsafePartial fromJust a <> ", World") (unsafePartial fromJust a <> ", Universe")

test3 :: Int -> Boolean
test3 x = do
  let a = if x > 42 then Just true else Nothing
  unsafePartial fromJust a && not (unsafePartial fromJust a)
