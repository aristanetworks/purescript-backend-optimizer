-- @inline export mapU arity=1
-- @inline export filterMapU arity=1
-- @inline export filterU arity=1
-- @inline export fromArray arity=1
-- @inline export toArray arity=1
-- @inline export overArray arity=1
module Snapshot.Fusion02 (test) where

import Prelude

import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Partial.Unsafe (unsafePartial)

type Step a s = forall r. (Unit -> r) -> (s -> a -> r) -> r
data Unfold' a s = Unfold s (s -> Step a s)
type Unfold a = Exists (Unfold' a)

mapU :: forall a b. (a -> b) -> Unfold a -> Unfold b
mapU f = runExists \(Unfold s1 step) ->
  mkExists $ Unfold s1 \s2 nothing just ->
    step s2 nothing \s3 a ->
      just s3 (f a)

filterMapU :: forall a b. (a -> Maybe b) -> Unfold a -> Unfold b
filterMapU f = runExists \(Unfold s1 step) ->
  mkExists $ Unfold s1 \s2 nothing just -> do
    let
      loop s3 =
        step s3 nothing \s4 a ->
          case f a of
            Nothing ->
              loop s4
            Just b ->
              just s4 b
    loop s2

filterU :: forall a. (a -> Boolean) -> Unfold a -> Unfold a
filterU p = filterMapU \a -> if p a then Just a else Nothing

fromArray :: forall a. Array a -> Unfold a
fromArray arr = mkExists $ Unfold 0 \ix nothing just ->
  if ix == Array.length arr then
    nothing unit
  else
    just (ix + 1) (unsafePartial Array.unsafeIndex arr ix)

toArray :: forall a. Unfold a -> Array a
toArray = runExists \(Unfold s1 step) -> do
  let
    loop s2 acc =
      step s2
        (\_ -> Array.reverse (List.toUnfoldable acc))
        (\s3 a -> loop s3 (List.Cons a acc))
  loop s1 List.Nil

overArray :: forall a b. (Unfold a -> Unfold b) -> Array a -> Array b
overArray unfold = toArray <<< unfold <<< fromArray

test :: Array Int -> Array String
test = overArray do
  mapU (add 1)
    >>> mapU show
    >>> filterMapU (String.stripPrefix (String.Pattern "1"))
    >>> mapU (append "2")
    >>> filterU (_ /= "wat")
    >>> mapU (flip append "1")
