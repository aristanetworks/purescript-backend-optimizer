module PureScript.CoreFn.Sort where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, foldr)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import PureScript.CoreFn (Import(..), Module(..), moduleName)

sortModules :: forall f a. Foldable f => f (Module a) -> List (Module a)
sortModules =
  ( \init -> do
      let modIndex = foldl (flip (uncurry Map.insert <<< initialModuleWithName)) Map.empty init
      let modStk = Right <<< moduleName <$> List.fromFoldable init
      go modIndex List.Nil modStk
  )
  where
  initialModuleWithName mod@(Module { name }) =
    Tuple name { visited: false, module: mod }

  go modIndex acc = case _ of
    List.Cons (Left mod) names ->
      go modIndex (List.Cons mod acc) names
    List.Cons (Right name) names ->
      case Map.lookup name modIndex of
        Just modState@{ module: Module mod } -> do
          if modState.visited then
            go modIndex acc names
          else do
            let importNames = (\(Import _ imp) -> Right imp) <$> mod.imports
            let modIndex' = Map.insert name (modState { visited = true }) modIndex
            go modIndex' acc (foldr List.Cons (List.Cons (Left modState.module) names) importNames)
        _ ->
          go modIndex acc names
    List.Nil ->
      List.reverse acc
