module PureScript.Backend.Optimizer.Codegen.EcmaScript.Inline where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Convert (CodegenEnv, CodegenRefType(..), InlineAppSpine(..), codegenBlockStatements, codegenExpr, effectLoopMode, freshName)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax (EsBindingPattern(..), EsExpr, EsObjectElement(..), EsRuntimeOp(..), EsSyntax(..), EsUnaryOp(..), build, toEsIdent)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoExpr(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Prop(..), Qualified)
import PureScript.Backend.Optimizer.Semantics.Foreign (qualified)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

type EsInline = Tuple (Qualified Ident) EsInlineCall
type EsInlineCall = CodegenEnv -> Qualified Ident -> InlineAppSpine -> Maybe EsExpr
type EsInlineMap = Map (Qualified Ident) EsInlineCall

esInlineMap :: EsInlineMap
esInlineMap = Map.fromFoldable
  [ control_monad_st_internal_for
  , control_monad_st_internal_foreach
  , control_monad_st_internal_run
  , control_monad_st_internal_while
  , data_array_st_new
  , data_array_st_pushAll
  , data_array_st_unshiftAll
  , effect_forE
  , effect_foreachE
  , effect_whileE
  , effect_untilE
  , effect_unsafe_unsafePerformEffect
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
    InlineEffectApp [] ->
      Just $ build $ EsArray []
    _ ->
      Nothing

data_array_st_pushAll :: EsInline
data_array_st_pushAll = Tuple (qualified "Data.Array.ST" "pushAll") $ arraySTAll "push"

data_array_st_unshiftAll :: EsInline
data_array_st_unshiftAll = Tuple (qualified "Data.Array.ST" "unshiftAll") $ arraySTAll "unshift"

arraySTAll :: String -> EsInlineCall
arraySTAll method env _ = case _ of
  InlineEffectApp [ TcoExpr _ (Lit (LitArray vals)), arr ] ->
    Just $ build $ EsCall (build (EsAccess (codegenExpr env arr) method)) $ codegenExpr env <$> vals
  _ ->
    Nothing

effect_forE :: EsInline
effect_forE = Tuple (qualified "Effect" "forE") forRangeLoop

effect_foreachE :: EsInline
effect_foreachE = Tuple (qualified "Effect" "foreachE") foreachLoop

effect_whileE :: EsInline
effect_whileE = Tuple (qualified "Effect" "whileE") whileLoop

effect_untilE :: EsInline
effect_untilE = Tuple (qualified "Effect" "untilE") untilLoop

effect_unsafe_unsafePerformEffect :: EsInline
effect_unsafe_unsafePerformEffect  = Tuple (qualified "Effect.Unsafe" "unsafePerformEffect") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ build $ EsCall (codegenExpr env eff) []
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
  InlineEffectApp [ lo, hi, TcoExpr _ (Abs args body) ] | [ Tuple ident lvl ] <- NonEmptyArray.toArray args -> do
    let Tuple ident' env' = freshName RefStrict ident lvl env
    let loopHead = build $ EsRuntime (EsRange (codegenExpr env' lo) (codegenExpr env' hi))
    let loopBody = codegenBlockStatements effectLoopMode env' body
    Just $ build $ EsForOf (EsBindingIdent (toEsIdent ident')) loopHead loopBody
  _ ->
    Nothing

foreachLoop :: EsInlineCall
foreachLoop env _ = case _ of
  InlineEffectApp [ arr, TcoExpr _ (Abs args body) ] | [ Tuple ident lvl ] <- NonEmptyArray.toArray args -> do
    let Tuple ident' env' = freshName RefStrict ident lvl env
    let loopHead = codegenExpr env' arr
    let loopBody = codegenBlockStatements effectLoopMode env' body
    Just $ build $ EsForOf (EsBindingIdent (toEsIdent ident')) loopHead loopBody
  InlineApp [ arr, TcoExpr _ (Abs args body) ] | [ Tuple ident lvl ] <- NonEmptyArray.toArray args -> do
    let Tuple ident' env' = freshName RefStrict ident lvl env
    let loopHead = codegenExpr env' arr
    let loopBody = codegenBlockStatements effectLoopMode env' body
    Just $ build $ EsArrowFunction [] [ build $ EsForOf (EsBindingIdent (toEsIdent ident')) loopHead loopBody ]
  _ ->
    Nothing

whileLoop :: EsInlineCall
whileLoop env _ = case _ of
  InlineEffectApp [ cond, body ] -> do
    let loopHead = build $ EsCall (codegenExpr env cond) []
    let loopBody = codegenBlockStatements effectLoopMode env body
    Just $ build $ EsWhile loopHead loopBody
  _ ->
    Nothing

untilLoop :: EsInlineCall
untilLoop env _ = case _ of
  InlineEffectApp [ eff ] -> do
    let loopHead = build $ EsCall (codegenExpr env eff) []
    Just $ build $ EsWhile (build (EsUnary EsNot loopHead)) []
  _ ->
    Nothing
