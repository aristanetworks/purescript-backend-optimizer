module PureScript.Backend.Optimizer.Codegen.EcmaScript.Inline where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Common (EsStatement(..), esUpdateLeft, esUpdateRight)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoExpr(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Qualified)
import PureScript.Backend.Optimizer.Semantics.Foreign (qualified)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

type EsInline a = Tuple (Qualified Ident) (EsInlineCall a)
type EsInlineCall a = (TcoExpr -> Dodo.Doc a) -> Qualified Ident -> Array TcoExpr -> Maybe (Tuple (Dodo.Doc a) (EsStatement (Dodo.Doc a)))
type EsInlineMap a = Map (Qualified Ident) (EsInlineCall a)

esInlineMap :: forall a. EsInlineMap a
esInlineMap = Map.fromFoldable
  [ record_unsafe_union_unsafeUnionFn
  ]

record_unsafe_union_unsafeUnionFn :: forall a. EsInline a
record_unsafe_union_unsafeUnionFn = Tuple (qualified "Record.Unsafe.Union" "unsafeUnionFn") go
  where
  go codegenExpr _ = case _ of
    [ lhs, TcoExpr _ (Lit (LitRecord props)) ] -> do
      let doc = esUpdateRight (map codegenExpr <$> props) (codegenExpr lhs)
      Just $ Tuple doc (ReturnObject doc)
    [ TcoExpr _ (Lit (LitRecord props)), rhs ] -> do
      let doc = esUpdateLeft (codegenExpr rhs) (map codegenExpr <$> props)
      Just $ Tuple doc (ReturnObject doc)
    _ ->
      Nothing
