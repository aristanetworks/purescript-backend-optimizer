module Snapshot.Comparison01 where

import Prelude

import Data.Comparison (Comparison, defaultComparison)
import Data.Functor.Contravariant ((>$<))
import Data.Tuple (Tuple, fst, snd)

test1 = defaultComparison :: Comparison { foo :: Int }

test2 = _.foo >$< (defaultComparison :: Comparison { foo :: Int })

test3 =
  (_.foo >$< defCom)
    <> (_.bar >$< defCom)
    <> (_.baz >$< defCom)
  where
  defCom :: Comparison { foo :: Int, bar :: String, baz :: Boolean }
  defCom = defaultComparison

test4 = mempty :: Comparison String

test5 = defaultComparison :: Comparison Int

test6 = _.foo >$< (defaultComparison :: Comparison Int)

test7 =
  (fst >$< defCom)
    <> (fst <<< snd >$< defCom)
    <> (snd <<< snd >$< defCom)
  where
  defCom :: Comparison (Tuple Int (Tuple String Boolean))
  defCom = defaultComparison

test8 = mempty :: Comparison String
