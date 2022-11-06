module PureScript.Backend.Optimizer.Codegen.EcmaScript.Inline where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax (EsExpr, EsObjectElement(..), EsSyntax(..), build)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoExpr(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Prop(..), Qualified)
import PureScript.Backend.Optimizer.Semantics.Foreign (qualified)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

type EsInline = Tuple (Qualified Ident) EsInlineCall
type EsInlineCall = (TcoExpr -> EsExpr) -> Qualified Ident -> Array TcoExpr -> Maybe EsExpr
type EsInlineMap = Map (Qualified Ident) EsInlineCall

esInlineMap :: EsInlineMap
esInlineMap = Map.fromFoldable
  [ record_unsafe_union_unsafeUnionFn
  ]

record_unsafe_union_unsafeUnionFn :: EsInline
record_unsafe_union_unsafeUnionFn = Tuple (qualified "Record.Unsafe.Union" "unsafeUnionFn") go
  where
  go codegenExpr _ = case _ of
    [ lhs, TcoExpr _ (Lit (LitRecord props)) ] -> do
      Just $ build $ EsObject $ Array.snoc
        ((\(Prop a b) -> EsObjectField a (codegenExpr b)) <$> props)
        (EsObjectSpread (codegenExpr lhs))
    [ TcoExpr _ (Lit (LitRecord props)), rhs ] -> do
      Just $ build $ EsObject $ Array.cons
        (EsObjectSpread (codegenExpr rhs))
        ((\(Prop a b) -> EsObjectField a (codegenExpr b)) <$> props)
    _ ->
      Nothing
