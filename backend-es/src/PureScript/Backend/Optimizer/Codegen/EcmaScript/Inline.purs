module PureScript.Backend.Optimizer.Codegen.EcmaScript.Inline where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Convert (CodegenEnv, CodegenRefType(..), InlineSpine(..), codegenBlockStatements, codegenExpr, effectLoopMode, freshName)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax (EsArrayElement(..), EsBindingPattern(..), EsExpr(..), EsIdent(..), EsObjectElement(..), EsRuntimeOp(..), EsSyntax(..), EsUnaryOp(..), build, toEsIdent)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoExpr(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Interned (Interned(..))
import PureScript.Backend.Optimizer.Semantics.Foreign (qualified)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

type EsInline = Tuple (Interned (Qualified Ident)) EsInlineCall
type EsInlineCall = CodegenEnv -> Interned (Qualified Ident) -> InlineSpine TcoExpr -> Maybe EsExpr
type EsInlineMap = Map (Interned (Qualified Ident)) EsInlineCall

esInlineMap :: EsInlineMap
esInlineMap = Map.fromFoldable
  [ control_monad_st_internal_for
  , control_monad_st_internal_foreach
  , control_monad_st_internal_run
  , control_monad_st_internal_while
  , data_array_st_new
  , data_array_st_pushAll
  , data_array_st_unshiftAll
  , data_semigroup_concatArray
  , effect_forE
  , effect_foreachE
  , effect_whileE
  , effect_unsafe_unsafePerformEffect
  , foreign_object_copyST
  , foreign_object_keys
  , foreign_object_member
  , foreign_object_runST
  , foreign_object_st_new
  , foreign_object_st_delete
  , foreign_object_st_poke
  , foreign_object_unsafe_unsafeIndex
  , record_unsafe_union_unsafeUnionFn
  ]

control_monad_st_internal_run :: EsInline
control_monad_st_internal_run = Tuple (qualified "Control.Monad.ST.Internal" "run") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ build $ EsCall (codegenExpr env eff) []
    _ ->
      Nothing

control_monad_st_internal_for :: EsInline
control_monad_st_internal_for = Tuple (qualified "Control.Monad.ST.Internal" "for") forRangeLoop

control_monad_st_internal_foreach :: EsInline
control_monad_st_internal_foreach = Tuple (qualified "Control.Monad.ST.Internal" "foreach") foreachLoop

control_monad_st_internal_while :: EsInline
control_monad_st_internal_while = Tuple (qualified "Control.Monad.ST.Internal" "while") whileLoop

data_array_st_new :: EsInline
data_array_st_new = Tuple (qualified "Data.Array.ST" "new") go
  where
  go _ _ = case _ of
    InlineApp [] ->
      Just $ makeThunk $ build $ EsArray []
    _ ->
      Nothing

data_array_st_pushAll :: EsInline
data_array_st_pushAll = Tuple (qualified "Data.Array.ST" "pushAll") $ arraySTAll "push"

data_array_st_unshiftAll :: EsInline
data_array_st_unshiftAll = Tuple (qualified "Data.Array.ST" "unshiftAll") $ arraySTAll "unshift"

arraySTAll :: String -> EsInlineCall
arraySTAll method env _ = case _ of
  InlineApp [ TcoExpr _ (Lit (LitArray vals)), arr ] ->
    Just $ makeThunk $ build $ EsCall (build (EsAccess (codegenExpr env arr) method)) $ EsArrayValue <<< codegenExpr env <$> vals
  InlineApp [ vals, arr ] ->
    Just $ makeThunk $ build $ EsCall (build (EsAccess (codegenExpr env arr) method)) $ spreadConcatArray $ codegenExpr env vals
  _ ->
    Nothing

data_semigroup_concatArray :: EsInline
data_semigroup_concatArray = Tuple (qualified "Data.Semigroup" "concatArray") go
  where
  go env _ = case _ of
    InlineApp [ a, b ] ->
      Just $ build $ EsArray $ spreadConcatArray (codegenExpr env a) <> spreadConcatArray (codegenExpr env b)
    _ ->
      Nothing

effect_forE :: EsInline
effect_forE = Tuple (qualified "Effect" "forE") forRangeLoop

effect_foreachE :: EsInline
effect_foreachE = Tuple (qualified "Effect" "foreachE") foreachLoop

effect_whileE :: EsInline
effect_whileE = Tuple (qualified "Effect" "whileE") whileLoop

effect_unsafe_unsafePerformEffect :: EsInline
effect_unsafe_unsafePerformEffect = Tuple (qualified "Effect.Unsafe" "unsafePerformEffect") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ build $ EsCall (codegenExpr env eff) []
    _ ->
      Nothing

foreign_object_copyST :: EsInline
foreign_object_copyST = Tuple (qualified "Foreign.Object" "_copyST") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ makeThunk $ build $ EsObject [ EsObjectSpread (codegenExpr env eff) ]
    _ ->
      Nothing

foreign_object_keys :: EsInline
foreign_object_keys = Tuple (qualified "Foreign.Object" "keys") go
  where
  go env _ = case _ of
    InlineApp [ a ] ->
      Just $ build $ EsCall (build (EsAccess (build (EsIdent (Qualified Nothing (Generated "Object")))) "keys"))
        [ EsArrayValue $ codegenExpr env a ]
    _ ->
      Nothing

foreign_object_member :: EsInline
foreign_object_member = Tuple (qualified "Foreign.Object" "member") go
  where
  go env _ = case _ of
    InlineApp [ a, b ] ->
      Just $ build $ EsCall (build (EsAccess (build (EsIdent (Qualified Nothing (Generated "Object")))) "hasOwn"))
        [ EsArrayValue $ codegenExpr env b
        , EsArrayValue $ codegenExpr env a
        ]
    _ ->
      Nothing

foreign_object_runST :: EsInline
foreign_object_runST = Tuple (qualified "Foreign.Object" "runST") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ build $ EsCall (codegenExpr env eff) []
    _ ->
      Nothing

foreign_object_st_new :: EsInline
foreign_object_st_new = Tuple (qualified "Foreign.Object.ST" "new") go
  where
  go _ _ = case _ of
    InlineApp [] ->
      Just $ makeThunk $ build $ EsObject []
    _ ->
      Nothing

foreign_object_st_delete :: EsInline
foreign_object_st_delete = Tuple (qualified "Foreign.Object.ST" "delete") go
  where
  go env _ = case _ of
    InlineApp [ TcoExpr _ (Lit (LitString key)), b ] ->
      Just $ makeThunk $ build $ EsUnary EsDelete $ build (EsAccess (codegenExpr env b) key)
    InlineApp [ a, b ] ->
      Just $ makeThunk $ build $ EsUnary EsDelete $ build (EsIndex (codegenExpr env b) (codegenExpr env a))
    _ ->
      Nothing

foreign_object_st_poke :: EsInline
foreign_object_st_poke = Tuple (qualified "Foreign.Object.ST" "poke") go
  where
  go env _ = case _ of
    InlineApp [ TcoExpr _ (Lit (LitString key)), b, c ] ->
      Just $ makeThunk $ build $ EsAssign (build (EsAccess (codegenExpr env c) key)) $ codegenExpr env b
    InlineApp [ a, b, c ] ->
      Just $ makeThunk $ build $ EsAssign (build (EsIndex (codegenExpr env c) (codegenExpr env a))) $ codegenExpr env b
    _ ->
      Nothing

foreign_object_unsafe_unsafeIndex :: EsInline
foreign_object_unsafe_unsafeIndex = Tuple (qualified "Foreign.Object.Unsafe" "unsafeIndex") go
  where
  go env _ = case _ of
    InlineApp [ obj, TcoExpr _ (Lit (LitString key)) ] ->
      Just $ build $ EsAccess (codegenExpr env obj) key
    InlineApp [ obj, key ] ->
      Just $ build $ EsIndex (codegenExpr env obj) (codegenExpr env key)
    _ ->
      Nothing

record_unsafe_union_unsafeUnionFn :: EsInline
record_unsafe_union_unsafeUnionFn = Tuple (qualified "Record.Unsafe.Union" "unsafeUnionFn") go
  where
  go env _ = case _ of
    InlineApp [ lhs, TcoExpr _ (Lit (LitRecord props)) ] -> do
      Just $ build $ EsObject $ Array.snoc
        ((\(Prop a b) -> EsObjectField a (codegenExpr env b)) <$> props)
        (EsObjectSpread (codegenExpr env lhs))
    InlineApp [ TcoExpr _ (Lit (LitRecord props)), rhs ] -> do
      Just $ build $ EsObject $ Array.cons
        (EsObjectSpread (codegenExpr env rhs))
        ((\(Prop a b) -> EsObjectField a (codegenExpr env b)) <$> props)
    _ ->
      Nothing

forRangeLoop :: EsInlineCall
forRangeLoop env _ = case _ of
  InlineApp [ lo, hi, TcoExpr _ (Abs args body) ]
    | [ Tuple ident lvl ] <- NonEmptyArray.toArray args -> do
        let Tuple ident' env' = freshName RefStrict ident lvl env
        let loopHead = build $ EsRuntime (EsRange (codegenExpr env' lo) (codegenExpr env' hi))
        let loopBody = codegenBlockStatements effectLoopMode env' body
        Just $ makeUnitThunk $ build $ EsForOf (EsBindingIdent (toEsIdent ident')) loopHead loopBody
  _ ->
    Nothing

foreachLoop :: EsInlineCall
foreachLoop env _ = case _ of
  InlineApp [ arr, TcoExpr _ (Abs args body) ]
    | [ Tuple ident lvl ] <- NonEmptyArray.toArray args -> do
        let Tuple ident' env' = freshName RefStrict ident lvl env
        let loopHead = codegenExpr env' arr
        let loopBody = codegenBlockStatements effectLoopMode env' body
        Just $ makeUnitThunk $ build $ EsForOf (EsBindingIdent (toEsIdent ident')) loopHead loopBody
  _ ->
    Nothing

whileLoop :: EsInlineCall
whileLoop env _ = case _ of
  InlineApp [ cond, body ] -> do
    let loopHead = build $ EsCall (codegenExpr env cond) []
    let loopBody = codegenBlockStatements effectLoopMode env body
    Just $ makeUnitThunk $ build $ EsWhile loopHead loopBody
  _ ->
    Nothing

makeThunk :: EsExpr -> EsExpr
makeThunk = build <<< EsArrowFunction [] <<< pure <<< build <<< EsReturn <<< Just

makeUnitThunk :: EsExpr -> EsExpr
makeUnitThunk = build <<< EsArrowFunction [] <<< pure

spreadConcatArray :: EsExpr -> Array (EsArrayElement EsExpr)
spreadConcatArray = go <=< flattenBinCall (qualified "Data.Semigroup" "concatArray")
  where
  go = case _ of
    EsExpr _ (EsArray elems) ->
      elems
    expr ->
      [ EsArraySpread expr ]

flattenBinCall :: Interned (Qualified Ident) -> EsExpr -> Array EsExpr
flattenBinCall qual = go
  where
  go :: EsExpr -> Array EsExpr
  go expr = case expr of
    EsExpr _ (EsCall (EsExpr _ (EsCall fn [ EsArrayValue lhs ])) [ EsArrayValue rhs ])
      | isEsIdent qual fn ->
          go lhs <> go rhs
    _ ->
      [ expr ]

isEsIdent :: Interned (Qualified Ident) -> EsExpr -> Boolean
isEsIdent (Interned _ (Qualified qual1 a)) = case _ of
  EsExpr _ (EsIdent (Qualified qual2 (Embedded b ""))) ->
    qual1 == qual2 && a == b
  _ ->
    false
