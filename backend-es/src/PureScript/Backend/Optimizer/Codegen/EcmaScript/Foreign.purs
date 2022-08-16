module PureScript.Backend.Optimizer.Codegen.EcmaScript.Foreign where

import Prelude

import Data.Enum (fromEnum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Qualified)
import PureScript.Backend.Optimizer.Semantics (BackendSemantics(..), ExternSpine(..))
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval, ForeignSemantics, qualified)

esForeignSemantics :: Map (Qualified Ident) ForeignEval
esForeignSemantics = Map.fromFoldable semantics
  where
  semantics =
    [ data_bounded_topInt
    , data_bounded_bottomInt
    , data_bounded_topChar
    , data_bounded_bottomChar
    , data_enum_toCharCode
    ]

data_bounded_topInt :: ForeignSemantics
data_bounded_topInt = Tuple (qualified "Data.Bounded" "topInt") go
  where
  go _ _ = const $ Just $ litInt top

data_bounded_bottomInt :: ForeignSemantics
data_bounded_bottomInt = Tuple (qualified "Data.Bounded" "bottomInt") go
  where
  go _ _ = const $ Just $ litInt bottom

data_bounded_topChar :: ForeignSemantics
data_bounded_topChar = Tuple (qualified "Data.Bounded" "topChar") go
  where
  go _ _ = const $ Just $ litChar top

data_bounded_bottomChar :: ForeignSemantics
data_bounded_bottomChar = Tuple (qualified "Data.Bounded" "bottomChar") go
  where
  go _ _ = const $ Just $ litChar bottom

data_enum_toCharCode :: ForeignSemantics
data_enum_toCharCode = Tuple (qualified "Data.Enum" "toCharCode") go
  where
  go _ _ = case _ of
    [ ExternApp [ NeutLit (LitChar c) ] ] ->
      Just $ NeutLit $ LitInt $ fromEnum c
    _ ->
      Nothing

litInt :: Int -> BackendSemantics
litInt = NeutLit <<< LitInt

litChar :: Char -> BackendSemantics
litChar = NeutLit <<< LitChar
