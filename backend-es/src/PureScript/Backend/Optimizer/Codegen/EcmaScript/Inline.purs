module PureScript.Backend.Optimizer.Codegen.EcmaScript.Inline where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Common (esApp)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoExpr(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Qualified)
import PureScript.Backend.Optimizer.Semantics.Foreign (qualified)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

type EsInline a = Tuple (Qualified Ident) (EsInlineCall a)
type EsInlineCall a = (TcoExpr -> Dodo.Doc a) -> Qualified Ident -> Array TcoExpr -> Maybe (Dodo.Doc a)
type EsInlineMap a = Map (Qualified Ident) (EsInlineCall a)

esInlineMap :: forall a. EsInlineMap a
esInlineMap = Map.fromFoldable
  [ record_unsafe_union_unsafeUnionFn
  ]

record_unsafe_union_unsafeUnionFn :: forall a. EsInline a
record_unsafe_union_unsafeUnionFn = Tuple (qualified "Record.Unsafe.Union" "unsafeUnionFn") go
  where
  go codegenExpr _ = case _ of
    [ lhs, rhs@(TcoExpr _ (Lit (LitRecord _))) ] ->
      Just $ esApp (Dodo.text "$runtime.recordUnionMutateRight")
        [ codegenExpr lhs
        , codegenExpr rhs
        ]
    [ lhs@(TcoExpr _ (Lit (LitRecord _))), rhs ] ->
      Just $ esApp (Dodo.text "$runtime.recordUnionMutateLeft")
        [ codegenExpr lhs
        , codegenExpr rhs
        ]
    _ ->
      Nothing
