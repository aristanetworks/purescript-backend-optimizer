-- @inline export overArray arity=1
module Snapshot.Fusion01 (test) where

import Prelude

import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Partial.Unsafe (unsafePartial)

newtype Fold a = Fold (forall r. (a -> r -> r) -> r -> r)

mapF :: forall a b. (a -> b) -> Fold a -> Fold b
mapF f (Fold next) = Fold \cons nil -> next (cons <<< f) nil

filterMapF :: forall a b. (a -> Maybe b) -> Fold a -> Fold b
filterMapF f (Fold next) = Fold \cons nil ->
  next
    ( \a as ->
        case f a of
          Just b ->
            cons b as
          Nothing ->
            as
    )
    nil

filterF :: forall a. (a -> Boolean) -> Fold a -> Fold a
filterF p = filterMapF (\a -> if p a then Just a else Nothing)

fromArray :: forall a. Array a -> Fold a
fromArray arr = Fold \cons nil -> do
  let
    loop n acc
      | n == 0 =
          acc
      | otherwise =
          loop (n - 1) $ cons (unsafePartial (Array.unsafeIndex arr n)) acc
  loop (Array.length arr - 1) nil

toArray :: forall a. Fold a -> Array a
toArray (Fold next) = Array.reverse $ List.toUnfoldable $ next List.Cons List.Nil

overArray :: forall a b. (Fold a -> Fold b) -> Array a -> Array b
overArray fold = toArray <<< fold <<< fromArray

test :: Array Int -> Array String
test = overArray do
  mapF (add 1)
    >>> mapF show
    >>> filterMapF (String.stripPrefix (String.Pattern "1"))
    >>> mapF (append "2")
    >>> filterF (_ /= "wat")
    >>> mapF (flip append "1")
