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
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval, ForeignSemantics, qualified)
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator2(..))
import PureScript.Backend.Optimizer.Utils (foldr1Array)

esForeignSemantics :: Map (Qualified Ident) ForeignEval
esForeignSemantics = Map.fromFoldable
  [ control_monad_st_internal_for
  , control_monad_st_internal_foreach
  , control_monad_st_internal_while
  , data_array_st_push
  , data_array_st_unshift
  , data_array_st_unsafeFreeze
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
  , effect_forE
  , effect_foreachE
  , effect_whileE
  , foreign_object_copyST
  , foreign_object_member
  , foreign_object_st_delete
  , foreign_object_st_poke
  , foreign_object_st_unsafe_unsafeFreeze
  ]

data_array_st_push :: ForeignSemantics
data_array_st_push = Tuple (qualified "Data.Array.ST" "push") $ arraySTAll (qualified "Data.Array.ST" "pushAll")

data_array_st_unshift :: ForeignSemantics
data_array_st_unshift = Tuple (qualified "Data.Array.ST" "unshift") $ arraySTAll (qualified "Data.Array.ST" "unshiftAll")

arraySTAll :: Qualified Ident -> ForeignEval
arraySTAll ident env _ = case _ of
  [ ExternApp [ val ] ] ->
    Just $
      makeLet Nothing val \nextVal ->
        SemLam Nothing \nextRef ->
          SemEffectDefer $
            evalApp 0 env (NeutStop ident) [ NeutLit (LitArray [ nextVal ]), nextRef ]
  _ ->
    Nothing

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
          let appendStrings l r = evalPrimOp 0 env (Op2 OpStringAppend l r)
          let showVal val = evalApp 0 env showDict [ val ]
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

control_monad_st_internal_for :: ForeignSemantics
control_monad_st_internal_for = Tuple (qualified "Control.Monad.ST.Internal" "for") forRangeLoop

control_monad_st_internal_foreach :: ForeignSemantics
control_monad_st_internal_foreach = Tuple (qualified "Control.Monad.ST.Internal" "foreach") foreachLoop

control_monad_st_internal_while :: ForeignSemantics
control_monad_st_internal_while = Tuple (qualified "Control.Monad.ST.Internal" "while") whileLoop

effect_forE :: ForeignSemantics
effect_forE = Tuple (qualified "Effect" "forE") forRangeLoop

effect_foreachE :: ForeignSemantics
effect_foreachE = Tuple (qualified "Effect" "foreachE") foreachLoop

effect_whileE :: ForeignSemantics
effect_whileE = Tuple (qualified "Effect" "whileE") whileLoop

foreign_object_copyST :: ForeignSemantics
foreign_object_copyST = Tuple (qualified "Foreign.Object" "_copyST") go
  where
  go env qual = case _ of
    [ ExternApp [ obj ] ] ->
      Just $
        makeLet Nothing obj \nextObj ->
          SemEffectDefer $ evalApp 0 env (NeutStop qual) [ nextObj ]
    _ ->
      Nothing

foreign_object_member :: ForeignSemantics
foreign_object_member = Tuple (qualified "Foreign.Object" "member") go
  where
  go _ qual = case _ of
    [] ->
      Just $ NeutStop qual
    _ ->
      Nothing

foreign_object_st_delete :: ForeignSemantics
foreign_object_st_delete = Tuple (qualified "Foreign.Object.ST" "delete") go
  where
  go env qual = case _ of
    [ ExternApp [ a ] ] ->
      Just $
        makeLet Nothing a \a' ->
          SemLam Nothing \b ->
            SemEffectBind Nothing (evalApp 0 env (NeutStop qual) [ a', b ]) \_ ->
              SemEffectPure b
    _ ->
      Nothing

foreign_object_st_poke :: ForeignSemantics
foreign_object_st_poke = Tuple (qualified "Foreign.Object.ST" "poke") go
  where
  go env qual = case _ of
    [ ExternApp [ a, b ] ] ->
      Just $
        makeLet Nothing a \a' ->
          makeLet Nothing b \b' ->
            SemLam Nothing \c ->
              SemEffectBind Nothing (evalApp 0 env (NeutStop qual) [ a', b', c ]) \_ ->
                SemEffectPure c
    _ ->
      Nothing

foreign_object_st_unsafe_unsafeFreeze :: ForeignSemantics
foreign_object_st_unsafe_unsafeFreeze = Tuple (qualified "Foreign.Object.ST.Unsafe" "unsafeFreeze") unsafeSTCoerce

forRangeLoop :: ForeignEval
forRangeLoop env qual = case _ of
  [ ExternApp [ a, b, SemLam ident c ] ] ->
    Just $
      makeLet Nothing a \a' ->
        makeLet Nothing b \b' ->
          SemEffectDefer $ evalApp 0 env (NeutStop qual)
            [ a', b', SemLam ident (SemEffectDefer <<< c) ]
  [ ExternApp [ a, b, c ] ] ->
    Just $
      makeLet Nothing a \a' ->
        makeLet Nothing b \b' ->
          makeLet Nothing c \c' ->
            SemEffectDefer $ evalApp 0 env (NeutStop qual)
              [ a', b', SemLam Nothing (SemEffectDefer <<< evalApp 0 env c' <<< pure) ]
  _ ->
    Nothing

foreachLoop :: ForeignEval
foreachLoop env qual = case _ of
  [ ExternApp [ a, SemLam ident b ] ] ->
    Just $ makeLet Nothing a \a' ->
      SemEffectDefer $ evalApp 0 env (NeutStop qual)
        [ a', SemLam ident (SemEffectDefer <<< b) ]
  [ ExternApp [ a, b ] ] ->
    Just $
      makeLet Nothing a \a' ->
        makeLet Nothing b \b' ->
          SemEffectDefer $ evalApp 0 env (NeutStop qual)
            [ a', SemLam Nothing (SemEffectDefer <<< evalApp 0 env b' <<< pure) ]
  _ ->
    Nothing

whileLoop :: ForeignEval
whileLoop env qual = case _ of
  [ ExternApp [ a, b ] ] ->
    Just $
      makeLet Nothing a \a' ->
        makeLet Nothing b \b' ->
          SemEffectDefer $ evalApp 0 env (NeutStop qual) [ a', SemEffectDefer b' ]
  _ ->
    Nothing

litInt :: Int -> BackendSemantics
litInt = NeutLit <<< LitInt

litChar :: Char -> BackendSemantics
litChar = NeutLit <<< LitChar

litString :: String -> BackendSemantics
litString = NeutLit <<< LitString
