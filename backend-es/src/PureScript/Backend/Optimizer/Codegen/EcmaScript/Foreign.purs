module PureScript.Backend.Optimizer.Codegen.EcmaScript.Foreign where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Enum (fromEnum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Qualified)
import PureScript.Backend.Optimizer.Semantics (BackendSemantics(..), ExternSpine(..), evalApp, evalPrimOp, makeLet)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignSemantics, ForeignEval, qualified)
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator2(..))
import PureScript.Backend.Optimizer.Utils (foldr1Array)

esForeignSemantics :: Map (Qualified Ident) ForeignEval
esForeignSemantics = Map.fromFoldable
  [ data_array_st_unsafeFreeze
  , data_array_st_unsafeThaw
  , data_bounded_topInt
  , data_bounded_bottomInt
  , data_bounded_topChar
  , data_bounded_bottomChar
  , data_enum_toCharCode
  , data_show_showCharImpl
  , data_show_showIntImpl
  , data_show_showNumberImpl
  , data_show_showStringImpl
  , data_show_showArrayImpl
  , data_unit_unit
  ]

data_array_st_unsafeFreeze :: ForeignSemantics
data_array_st_unsafeFreeze = Tuple (qualified "Data.Array.ST" "unsafeFreeze") unsafeSTCoerce

data_array_st_unsafeThaw :: ForeignSemantics
data_array_st_unsafeThaw = Tuple (qualified "Data.Array.ST" "unsafeThaw") unsafeSTCoerce

unsafeSTCoerce :: ForeignEval
unsafeSTCoerce _ _ = case _ of
  [ ExternApp [ ref ] ] ->
    Just $ makeLet Nothing ref \nextRef ->
      SemEffectPure nextRef
  _ ->
    Nothing

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

data_show_showStringImpl :: ForeignSemantics
data_show_showStringImpl = Tuple (qualified "Data.Show" "showStringImpl") $ showLit case _ of
  NeutLit (LitString n) -> Just n
  _ -> Nothing

data_show_showIntImpl :: ForeignSemantics
data_show_showIntImpl = Tuple (qualified "Data.Show" "showIntImpl") $ showLit case _ of
  NeutLit (LitInt n) -> Just n
  _ -> Nothing

data_show_showNumberImpl :: ForeignSemantics
data_show_showNumberImpl = Tuple (qualified "Data.Show" "showNumberImpl") $ showLit case _ of
  NeutLit (LitNumber n) -> Just n
  _ -> Nothing

data_show_showCharImpl :: ForeignSemantics
data_show_showCharImpl = Tuple (qualified "Data.Show" "showCharImpl") $ showLit case _ of
  NeutLit (LitChar n) -> Just n
  _ -> Nothing

data_show_showArrayImpl :: ForeignSemantics
data_show_showArrayImpl = Tuple (qualified "Data.Show" "showArrayImpl") go
  where
  go env _ = case _ of
    [ ExternApp [ showDict, NeutLit (LitArray arr) ] ] ->
      Just case NonEmptyArray.fromArray arr of
        Nothing ->
          litString "[]"
        Just nea -> do
          let appendStrings l r = evalPrimOp env (Op2 OpStringAppend l r)
          let showVal val = evalApp env showDict [ val ]
          let foldFn next acc = appendStrings (showVal next) $ appendStrings (litString ",") acc
          appendStrings (litString "[") $ foldr1Array foldFn (\last -> appendStrings (showVal last) (litString "]")) nea
    _ ->
      Nothing

data_unit_unit :: ForeignSemantics
data_unit_unit = Tuple (qualified "Data.Unit" "unit") go
  where
  go _ _ = case _ of
    [] ->
      Just NeutPrimUndefined
    _ ->
      Nothing

showLit :: forall a. Show a => (BackendSemantics -> Maybe a) -> ForeignEval
showLit match _ _ = case _ of
  [ ExternApp [ sem ] ] | Just val <- match sem ->
    Just $ NeutLit $ LitString (show val)
  _ ->
    Nothing

litInt :: Int -> BackendSemantics
litInt = NeutLit <<< LitInt

litChar :: Char -> BackendSemantics
litChar = NeutLit <<< LitChar

litString :: String -> BackendSemantics
litString = NeutLit <<< LitString
