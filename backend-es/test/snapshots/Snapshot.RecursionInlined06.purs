-- @inline Snapshot.RecursionInlined06.foldlArray always
-- @inline Snapshot.RecursionInlined06.foldlArray2 always
-- @inline Snapshot.RecursionInlined06.actualizeNuts always

-- Another "real-world" example from deku
module Snapshot.RecursionInlined06 where

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

newtype ActualizedKorok = ActualizedKorok (Either Unit { count :: Int })
newtype PureKorok = PureKorok ({ count :: Int } -> ActualizedKorok)
type Korok = Unit
newtype Nut = Nut (Either PureKorok Korok)

actualizeNuts :: Int -> Tuple (Array (Either ActualizedKorok Korok)) Int
actualizeNuts count = foldlArray
  ( \(Tuple arr n) -> case _ of
      Left (PureKorok k) -> do
        let korok@(ActualizedKorok ak) = k { count: n }
        Tuple
          (arr <> [ Left korok ])
          ( case ak of
              Left _ -> n
              Right { count: c } -> c
          )
      Right x -> Tuple (arr <> [ Right x ]) n
  )
  (Tuple [] (count + 1))
  [ Left $ PureKorok \{ count: c } -> ActualizedKorok $ Right { count: c } ]

test1 :: Tuple (Array (Either ActualizedKorok Korok)) Int
test1 = actualizeNuts 42