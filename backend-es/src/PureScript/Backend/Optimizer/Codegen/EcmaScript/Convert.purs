module PureScript.Backend.Optimizer.Codegen.EcmaScript.Convert where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.Codegen.EcmaScript (BlockMode, CodegenEnv, CodegenName, CodegenRefType(..), TcoBinding, TcoJoin, effectMode, esPureEnv, freshName, freshNames, lookupCtorMeta, pureMode, pushTcoJoin, renameLocal, renameTopLevel, toTcoBindings, toTcoJoin)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax (EsArrayElement(..), EsExpr(..), EsIdent(..), EsObjectElement(..), EsSyntax(..), build, esArrowFunction, esBinding, esCurriedFunction, fromIdent)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoAnalysis(..), TcoExpr(..))
import PureScript.Backend.Optimizer.CoreFn (ConstructorType(..), Ident(..), Literal(..), Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (CtorMeta)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendSyntax(..))

codegenExpr :: forall a. CodegenEnv -> TcoExpr -> EsExpr
codegenExpr env tcoExpr@(TcoExpr _ expr) = case expr of
  Var (Qualified (Just mn) ident) | mn == env.currentModule ->
    codegenName $ renameTopLevel ident env
  Var qual ->
    build $ EsIdent $ fromIdent <$> qual
  Local ident lvl ->
    codegenName (renameLocal ident lvl env)
  Lit lit ->
    codegenLit env lit
  App a bs ->
    case a of
      -- TcoExpr _ (Var qual)
      --   | Just (Tuple doc _) <- shouldInlineApp env qual (NonEmptyArray.toArray bs) ->
      --       esPureEnv env $ doc
      _ ->
        foldl
          ( \hd -> case _ of
              TcoExpr _ PrimUndefined ->
                build $ EsCall hd []
              arg ->
                build $ EsCall hd [ codegenExpr env arg ]
          )
          (codegenExpr env a)
          bs
  Abs idents body -> do
    let result = freshNames RefStrict env idents
    esCurriedFunction (NonEmptyArray.toArray result.value) (codegenBlockStatements pureMode result.accum body)
  UncurriedAbs idents body -> do
    let result = freshNames RefStrict env idents
    esArrowFunction result.value (codegenBlockStatements pureMode result.accum body)
  UncurriedApp a bs ->
    case a of
      -- TcoExpr _ (Var qual)
      --   | Just (Tuple doc _) <- shouldInlineApp env qual bs ->
      --       esPureEnv env $ doc
      _ ->
        build $ EsCall (codegenExpr env a) (codegenExpr env <$> bs)
  UncurriedEffectAbs idents body -> do
    let result = freshNames RefStrict env idents
    esArrowFunction result.value (codegenBlockStatements effectMode result.accum body)
  UncurriedEffectApp _ _ ->
    codegenEffectBlock env tcoExpr
  Accessor a (GetProp prop) ->
    build $ EsAccess (codegenExpr env a) prop
  Accessor a (GetOffset ix) ->
    build $ EsAccess (codegenExpr env a) ("_" <> show (ix + 1))
  Accessor a (GetIndex ix) ->
    build $ EsIndex (codegenExpr env a) ix
  Update a props ->
    build $ EsObject $ Array.cons (EsObjectSpread (codegenExpr env a)) $ codegenObjectElement env <$> props
  -- CtorDef ct ty tag fields ->
  --   esCurriedFunction (Ident <$> fields)
  --     [ EsReturn $ codegenCtor env (Qualified (Just env.currentModule) tag) $ ?go <$> fields ]
  _ ->
    ?b

codegenPureBlock :: CodegenEnv -> TcoExpr -> EsExpr
codegenPureBlock env a = build $ EsCall (esArrowFunction [] (codegenBlockStatements pureMode env a)) []

codegenEffectBlock :: CodegenEnv -> TcoExpr -> EsExpr
codegenEffectBlock env = esArrowFunction [] <<< codegenBlockStatements effectMode env

codegenBlockStatements :: BlockMode -> CodegenEnv -> TcoExpr -> Array EsExpr
codegenBlockStatements = go []
  where
  go acc mode env tcoExpr@(TcoExpr (TcoAnalysis analysis) expr) = case expr of
    LetRec lvl bindings body
      | Just tco <- toTcoBindings analysis.role bindings ->
          ?a
      | otherwise ->
          ?b
    Let ident lvl binding body
      | Just tco <- toTcoJoin mode.tcoScope analysis.role binding -> do
          let Tuple tcoIdent env' = freshName RefStrict ident lvl env
          let line = codegenTcoJoinBinding mode env tcoIdent tco
          go (Array.snoc acc line) (pushTcoJoin (Tuple ident lvl) mode) env' body
      | otherwise -> do
          let Tuple ident' env' = freshName RefStrict ident lvl env
          let line = esBinding ident' (codegenExpr env binding)
          go (Array.snoc acc line) mode env' body
    _ ->
      ?other

codegenTcoJoinBinding :: BlockMode -> CodegenEnv -> Ident -> TcoJoin -> EsExpr
codegenTcoJoinBinding mode env tcoIdent tco = do
  let result = freshNames RefStrict env tco.arguments
  let fn = if tco.curried then esCurriedFunction else esArrowFunction
  esBinding tcoIdent $ fn result.value $ codegenBlockStatements (mode { tco = false }) result.accum tco.body

codegenLit :: CodegenEnv -> Literal TcoExpr -> EsExpr
codegenLit env = case _ of
  LitInt n ->
    build $ EsInt n
  LitNumber n ->
    build $ EsNumber n
  LitString str ->
    build $ EsString str
  LitChar ch ->
    build $ EsString (SCU.singleton ch)
  LitBoolean bool ->
    build $ EsBoolean bool
  LitArray as ->
    build $ EsArray (EsArrayValue <<< codegenExpr env <$> as)
  LitRecord props ->
    build $ EsObject (codegenObjectElement env <$> props)

codegenObjectElement :: CodegenEnv -> Prop TcoExpr -> EsObjectElement EsExpr
codegenObjectElement env (Prop p1 expr) =
  case codegenExpr env expr of
    EsExpr _ (EsIdent (Qualified Nothing (Ident p2))) | p1 == p2 ->
      EsObjectPun p1
    other ->
      EsObjectField p1 other

-- codegenCtor :: forall a r. { intTags :: Boolean | r } -> ConstructorType -> Qualified Ident -> Ident -> CtorMeta -> Array (Dodo.Doc a) -> Dodo.Doc a
-- codegenCtor options ct fn ctor ctorMeta vals = case ct of
--   SumType ->
--     esApp (esCodegenQualified fn) $ Array.cons (esTag options ctor ctorMeta) vals
--   ProductType ->
--     esApp (esCodegenQualified fn) vals

-- codegenCtor :: CodegenEnv -> ConstructorType -> Maybe ModuleName -> EsIdent -> Ident -> Array EsExpr -> EsExpr
-- codegenCtor env ct mod fields = case ct of
--   SumType ->
--     build $ EsCall (build (EsIdent ident)) ()
--   ProductType ->
--     build $ EsCall (build (EsIdent ident)) fields
--   where

--   mod = case ctor of
--     Qualified (Just mn) id | mn == env.currentModule ->
--       Qualified Nothing id
--     _ ->
--       ctor

codegenTag :: CodegenEnv -> Ident -> CtorMeta -> EsExpr
codegenTag env (Ident ctor) { tag }
  | env.options.intTags =
      build $ EsCommentTrailing (build (EsInt tag)) ctor
  | otherwise =
      build $ EsString ctor

codegenName :: forall a. CodegenName -> EsExpr
codegenName (Tuple ident refType) = do
  let ident' = build $ EsIdent $ Qualified Nothing $ fromIdent ident
  case refType of
    RefStrict ->
      ident'
    RefLazy ->
      build $ EsCall ident' []
