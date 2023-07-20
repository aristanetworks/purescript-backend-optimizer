module Snapshot.KnownConstructors03 where

import Prelude

import Data.Maybe (Maybe(..))

test :: Int -> String
test x = do
  let a = if x > 42 then Just "Hello" else Nothing
  case a of
    Just str ->
      str <> ", World!"
    Nothing ->
      ""
