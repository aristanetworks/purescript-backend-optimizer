module PureScript.Backend.Optimizer.Hash where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Int.Bits (shl, shr, xor)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype Hash = Hash (Maybe Int)

toHash :: Int -> Hash
toHash = Hash <<< Just

mkHash :: forall a. Hashable a => a -> Hash
mkHash = Hash <<< Just <<< hash

-- don't add a hashable instance for hash!
-- we don't want to hash the maybe
derive instance Eq Hash
derive instance Ord Hash
derive instance Newtype Hash _
instance Semigroup Hash where
  append (Hash Nothing) b = b
  append a (Hash Nothing) = a
  append (Hash (Just a)) (Hash (Just b)) = Hash $ Just $ a `hashCombine'` b

instance Monoid Hash where
  mempty = Hash Nothing

type Hash' = Int

hashCombine :: Hash -> Hash -> Hash
hashCombine (Hash Nothing) b = b
hashCombine a (Hash Nothing) = a
hashCombine (Hash (Just a)) (Hash (Just b)) = Hash $ Just $ hashCombine' a b

hashCombine' :: Hash' -> Hash' -> Hash'
hashCombine' a b =
  -- combineWithSeed is based on the C++ boost::hash_combine.
  let
    combineWithSeed seed x = seed `xor` (x + 2147483647 + (seed `shl` 6) + (seed `shr` 2))
  in
    combineWithSeed (combineWithSeed a b) 0

thenHash :: forall a. Hashable a => Hash' -> a -> Hash'
thenHash xHash y = hashCombine' xHash (hash y)