module PureScript.Backend.Optimizer.CoreFn.Sort
  ( Pull
  , emptyPull
  , resumePull
  , pullResult
  , sortModules
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple (Tuple(..), snd)
import PureScript.Backend.Optimizer.CoreFn (Import(..), Module(..), ModuleName, importName, isPrimModule, moduleName)

runSort
  :: forall a
   . Map ModuleName (Tuple Boolean (Module a))
  -> List (Either (Module a) ModuleName)
  -> List (Module a)
runSort = go List.Nil
  where
  go acc modIndex = case _ of
    List.Cons (Left mod) stk ->
      go (List.Cons mod acc) modIndex stk
    List.Cons (Right name) stk
      | Just (Tuple visited mod@(Module { imports })) <- Map.lookup name modIndex
      , not visited -> do
          let modIndex' = Map.insert name (Tuple true mod) modIndex
          let stk' = foldr (List.Cons <<< Right <<< importName) (List.Cons (Left mod) stk) imports
          go acc modIndex' stk'
    List.Cons _ stk ->
      go acc modIndex stk
    List.Nil ->
      List.reverse acc

newtype Pull a = Pull
  { result :: Either (NonEmptySet ModuleName) (Lazy (List (Module a)))
  , resume :: Module a -> Pull a
  }

emptyPull :: forall a. Pull a
emptyPull = Pull { result: Right (pure List.Nil), resume: resume Map.empty Set.empty }
  where
  resume index needed mod@(Module { name, imports }) = do
    let
      index' =
        Map.insert name (Tuple false mod) index

      needed' = imports
        # Array.mapMaybe (\(Import _ mn) -> mn <$ guard (not (Map.member mn index' || isPrimModule mn)))
        # Set.fromFoldable
        # append (Set.delete name needed)

    case NonEmptySet.fromSet needed' of
      Just next ->
        Pull { result: Left next, resume: resume index' needed' }
      Nothing ->
        Pull
          { result:
              Right $ Lazy.defer \_ ->
                index'
                  # foldr (List.Cons <<< Right <<< moduleName <<< snd) List.Nil
                  # runSort index'
          , resume:
              resume index' needed'
          }

pullResult :: forall a. Pull a -> Either (NonEmptySet ModuleName) (Lazy (List (Module a)))
pullResult (Pull { result }) = result

resumePull :: forall a. Pull a -> Module a -> Pull a
resumePull (Pull { resume }) = resume

sortModules :: forall f a. Foldable f => f (Module a) -> List (Module a)
sortModules init = do
  let modIndex = foldr (\m -> Map.insert (moduleName m) (Tuple false m)) Map.empty init
  let modStk = foldr (List.Cons <<< Right <<< moduleName) List.Nil init
  runSort modIndex modStk
