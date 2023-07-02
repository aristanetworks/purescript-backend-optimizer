-- @inline Snapshot.RecursionInlined07.foldlArray always
-- @inline Snapshot.RecursionInlined07.foldlArray2 always
-- @inline Snapshot.RecursionInlined07.nutsToHtml always

-- Another "real-world" example from deku
module Snapshot.RecursionInlined07 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldlArray bab b arr = foldlArray2 0 len bab b arr
  where
  len = Array.length arr

foldlArray2 :: forall a b. Int -> Int -> (b -> a -> b) -> b -> Array a -> b
foldlArray2 n i bab b arr
  | n == i = b
  | otherwise = foldlArray2 (n + 1) i bab (bab b (unsafePartial $ Array.unsafeIndex arr n)) arr

newtype ActualizedKorok = ActualizedKorok (Either String { count :: Int, html :: String })
newtype PureKorok = PureKorok ({ count :: Int } -> ActualizedKorok)
type Korok = Unit
newtype Nut = Nut (Either PureKorok Korok)

delimiter :: String
delimiter = "qrs"

nutsToHtml :: Array (Either ActualizedKorok Korok) -> Int -> String
nutsToHtml actualized count = acc
  where
  (Tuple acc _) = foldlArray
    ( \(Tuple acc c) -> case _ of
        Left (ActualizedKorok k) -> case k of
          Left txt -> Tuple (acc <> txt) c
          Right { html } -> Tuple (acc <> html) c
        Right _ ->
          Tuple (acc <> delimiter <> show c <> delimiter) (c + 1)
    )
    (Tuple "" count)
    actualized

test1 :: String
test1 = nutsToHtml
  [ Left $ ActualizedKorok $ Right { count: 0, html: "<div></div>" }
  , Left $ ActualizedKorok $ Right { count: 0, html: "<h1></h1>" }
  , Left $ ActualizedKorok $ Right { count: 0, html: "<b></b>" }
  , Left $ ActualizedKorok $ Right { count: 0, html: "<i></i>" }
  ]
  0