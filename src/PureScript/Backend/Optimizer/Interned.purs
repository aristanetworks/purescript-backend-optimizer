-- @inline mkInterned never
module PureScript.Backend.Optimizer.Interned
  ( InternId
  , Interned(..)
  , makeInternableInstance
  , unInterned
  , class Internable
  , intern
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

newtype InternId = InternId Number

derive newtype instance Eq InternId
derive newtype instance Ord InternId

data Interned a = Interned InternId a

instance Eq (Interned a) where
  eq (Interned id1 _) (Interned id2 _) = id1 == id2

instance Ord (Interned a) where
  compare (Interned id1 _) (Interned id2 _) = compare id1 id2

mkInterned :: forall a. Fn2 InternId a (Interned a)
mkInterned = mkFn2 Interned

unInterned :: forall a. Interned a -> a
unInterned (Interned _ a) = a

makeInternableInstance :: forall a. Ord a => a -> Interned a
makeInternableInstance = unsafePerformEffect do
  ids <- Ref.new (-9007199254740991.0)
  ref <- Ref.new Map.empty
  pure \value -> unsafePerformEffect do
    values <- Ref.read ref
    case Map.lookup value values of
      Nothing -> do
        id <- Ref.modify (add 1.0) ids
        let interned = runFn2 mkInterned (InternId id) value
        Ref.write (Map.insert value interned values) ref
        pure interned
      Just interned ->
        pure interned

class Internable a where
  intern :: a -> Interned a
