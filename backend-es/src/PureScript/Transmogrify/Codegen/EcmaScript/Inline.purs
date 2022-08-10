module PureScript.Transmogrify.Codegen.EcmaScript.Inline where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import PureScript.Transmogrify.Codegen.EcmaScript.Common (esApp)
import PureScript.Transmogrify.Codegen.Tco (TcoExpr(..))
import PureScript.Transmogrify.Semantics.Foreign (qualified)
import PureScript.Transmogrify.Syntax (BackendSyntax(..))
import PureScript.Transmogrify.CoreFn (Ident, Literal(..), Qualified)

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
