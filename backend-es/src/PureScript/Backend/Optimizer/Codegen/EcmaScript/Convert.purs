module PureScript.Backend.Optimizer.Codegen.EcmaScript.Convert where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Foldable (all, fold, foldMap, foldl, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (maximum)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (class Traversable, Accum, mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Common (esEscapeIdent)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax (class ToEsIdent, EsArrayElement(..), EsBinaryOp(..), EsBindingPattern(..), EsExpr(..), EsIdent(..), EsObjectElement(..), EsRuntimeOp(..), EsSyntax(..), EsUnaryOp(..), build, esArrowFunction, esAssignIdent, esBinding, esCurriedFunction, esLazyBinding, printIdentString, toEsIdent, toEsIdentWith)
import PureScript.Backend.Optimizer.Codegen.Tco (LocalRef, TcoAnalysis(..), TcoExpr(..), TcoPop, TcoRef(..), TcoRole, TcoScope, TcoScopeItem, TcoUsage(..), tcoAnalysisOf)
import PureScript.Backend.Optimizer.Codegen.Tco as Tco
import PureScript.Backend.Optimizer.Convert (BackendBindingGroup, BackendImplementations)
import PureScript.Backend.Optimizer.CoreFn (ConstructorType(..), Ident(..), Literal(..), ModuleName, Prop(..), ProperName(..), Qualified(..), propValue, qualifiedModuleName, unQualified)
import PureScript.Backend.Optimizer.Semantics (CtorMeta, DataTypeMeta, ExternImpl(..), NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

data CodegenRefType = RefStrict | RefLazy | RefUnboxed

type CodegenName = Tuple Ident CodegenRefType

data CodegenRef
  = CodegenLocal (Maybe Ident) Level
  | CodegenTopLevel Ident

derive instance Eq CodegenRef
derive instance Ord CodegenRef

type CodegenOptions =
  { intTags :: Boolean
  }

newtype CodegenEnv = CodegenEnv
  { bound :: Map Ident Int
  , currentModule :: ModuleName
  , implementations :: BackendImplementations
  , inlineApp :: CodegenEnv -> Qualified Ident -> InlineSpine TcoExpr -> Maybe EsExpr
  , names :: Map CodegenRef CodegenName
  , options :: CodegenOptions
  }

data InlineSpine a
  = InlineApp (Array a)
  | InlineEffectApp (Array a)

type TcoBinding =
  { arguments :: NonEmptyArray (Tuple (Maybe Ident) Level)
  , body :: TcoExpr
  , name :: Ident
  }

type TcoJoin =
  { curried :: Boolean
  , arguments :: Array (Tuple (Maybe Ident) Level)
  , body :: TcoExpr
  }

data ReturnMode
  = Return
  | Discard
  | Continue

derive instance Eq ReturnMode

type BlockMode =
  { effect :: Boolean
  , return :: ReturnMode
  , tco :: Boolean
  , tcoScope :: TcoScope
  , tcoJoins :: Set LocalRef
  }

toTcoBinding :: Ident -> TcoExpr -> Maybe TcoBinding
toTcoBinding name = case _ of
  TcoExpr _ (Abs arguments body) ->
    Just { arguments, body, name }
  _ ->
    Nothing

toTcoBindings :: TcoRole -> NonEmptyArray (Tuple Ident TcoExpr) -> Maybe (NonEmptyArray TcoBinding)
toTcoBindings role bindings = do
  guard role.isLoop
  traverse (uncurry toTcoBinding) bindings

isTcoJoin :: TcoScope -> TcoRole -> Boolean
isTcoJoin tcoScope role = Array.any (flip Tco.inTcoScope tcoScope) role.joins

toTcoJoin :: TcoScope -> TcoRole -> TcoExpr -> Maybe TcoJoin
toTcoJoin tcoScope role = case _ of
  TcoExpr _ (Abs args body) | isTcoJoin tcoScope role ->
    Just { curried: true, arguments: NonEmptyArray.toArray args, body }
  TcoExpr _ (UncurriedAbs args body) | isTcoJoin tcoScope role ->
    Just { curried: false, arguments: args, body }
  _ ->
    Nothing

boundTopLevel :: Ident -> CodegenEnv -> CodegenEnv
boundTopLevel ident (CodegenEnv env) = CodegenEnv env { bound = Map.insert ident 1 env.bound }

genName :: CodegenEnv -> Tuple Ident CodegenEnv
genName (CodegenEnv env) =
  case Map.lookup (Ident "") env.bound of
    Nothing -> do
      let fresh = Ident "$0"
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident "") 1 env.bound
        }
    Just n -> do
      let fresh = Ident ("$" <> show n)
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident "") (n + 1) env.bound
        }

freshName :: CodegenRefType -> Maybe Ident -> Level -> CodegenEnv -> Tuple Ident CodegenEnv
freshName refType ident lvl (CodegenEnv env) = do
  let base = foldMap unwrap ident
  case Map.lookup (Ident base) env.bound of
    Nothing -> do
      let
        fresh
          | String.null base =
              Ident "$0"
          | otherwise =
              Ident base
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident base) 1 env.bound
        , names = Map.insert (CodegenLocal ident lvl) (Tuple fresh refType) env.names
        }
    Just n -> do
      let fresh = Ident (base <> "$" <> show n)
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident base) (n + 1) env.bound
        , names = Map.insert (CodegenLocal ident lvl) (Tuple fresh refType) env.names
        }

freshNames :: forall f. Traversable f => CodegenRefType -> CodegenEnv -> f LocalRef -> Accum CodegenEnv (f Ident)
freshNames refType = mapAccumL \env' (Tuple ident level) -> do
  let Tuple newIdent env'' = freshName refType ident level env'
  { accum: env'', value: newIdent }

freshBindingGroup :: forall f a. Traversable f => CodegenRefType -> Level -> CodegenEnv -> f (Tuple Ident a) -> Accum CodegenEnv (f (Tuple Ident a))
freshBindingGroup refType level = mapAccumL \env' (Tuple ident binding) -> do
  let Tuple newIdent env'' = freshName refType (Just ident) level env'
  { accum: env''
  , value: Tuple newIdent binding
  }

lazyTopLevel :: Ident -> CodegenEnv -> CodegenEnv
lazyTopLevel ident (CodegenEnv env) = CodegenEnv env { names = Map.insert (CodegenTopLevel ident) (Tuple ident RefLazy) env.names }

strictCodegenRef :: CodegenRef -> CodegenEnv -> CodegenEnv
strictCodegenRef ref (CodegenEnv env) = CodegenEnv env { names = Map.update (Just <<< (RefStrict <$ _)) ref env.names }

renameLocal :: Maybe Ident -> Level -> CodegenEnv -> CodegenName
renameLocal ident lvl (CodegenEnv env) =
  case Map.lookup (CodegenLocal ident lvl) env.names of
    Nothing ->
      Tuple (esLocalIdent ident lvl) RefStrict
    Just id ->
      id

renameTopLevel :: Ident -> CodegenEnv -> CodegenName
renameTopLevel ident (CodegenEnv env) = fromMaybe (Tuple ident RefStrict) $ Map.lookup (CodegenTopLevel ident) env.names

pureMode :: BlockMode
pureMode = { effect: false, return: Return, tco: false, tcoScope: List.Nil, tcoJoins: Set.empty }

effectMode :: BlockMode
effectMode = pureMode { effect = true }

effectLoopMode :: BlockMode
effectLoopMode = effectMode { return = Continue }

pushTcoScope :: TcoScopeItem -> BlockMode -> BlockMode
pushTcoScope scopeItem mode = mode { tco = true, tcoScope = List.Cons scopeItem mode.tcoScope }

pushTcoJoin :: LocalRef -> BlockMode -> BlockMode
pushTcoJoin ref mode = mode { tcoJoins = Set.insert ref mode.tcoJoins }

noTco :: BlockMode -> BlockMode
noTco = _ { tco = false, tcoScope = List.Nil, tcoJoins = Set.empty }

codegenTopLevelBindingGroup :: CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> Array EsExpr
codegenTopLevelBindingGroup env@(CodegenEnv { currentModule }) { bindings, recursive }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings = do
      let tcoGroup = Tco.topLevelTcoEnvGroup currentModule bindings'
      let bindings'' = map (Tco.analyze tcoGroup) <$> bindings'
      let tcoRefBindings = Tco.topLevelTcoRefBindings currentModule bindings''
      let isLoop = maybe false Tco.tcoRoleIsLoop tcoRefBindings
      case toTcoBindings { isLoop, joins: [] } bindings'' of
        Just tco -> do
          let tcoNames = _.name <$> tco
          let tcoIdent = asTcoMutualIdent tcoNames
          let tcoRefs = Tuple tcoIdent $ TcoTopLevel <<< Qualified (Just currentModule) <$> tcoNames
          let mode = pushTcoScope tcoRefs pureMode
          let env' = foldr boundTopLevel env tcoNames
          codegenTcoMutualLoopBindings mode (boundTopLevel tcoIdent env') tcoIdent (NonEmptyArray.zip tcoNames tco)
        Nothing -> do
          let group = CodegenTopLevel <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding currentModule group) bindings''
          let env' = foldr (lazyTopLevel <<< fst) env lazyBindings.no
          fold
            [ codegenBindings env' lazyBindings.yes
            , codegenLazyBindings env' lazyBindings.no
            , codegenLazyInits $ fst <$> lazyBindings.no
            ]
  | otherwise =
      codegenBindings env $ map (Tco.analyze []) <$> bindings

codegenExpr :: CodegenEnv -> TcoExpr -> EsExpr
codegenExpr env@(CodegenEnv { currentModule, inlineApp }) tcoExpr@(TcoExpr _ expr) = case expr of
  Var (Qualified (Just mn) ident) | mn == currentModule ->
    codegenName $ renameTopLevel ident env
  Var qual
    | Just expr' <- inlineApp env qual (InlineApp []) ->
        expr'
    | otherwise ->
        build $ EsIdent $ toEsIdent <$> qual
  Local ident lvl ->
    codegenName (renameLocal ident lvl env)
  Lit lit ->
    codegenLit env lit
  App a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- inlineApp env qual (InlineApp (NonEmptyArray.toArray bs)) ->
            expr'
      _ ->
        foldl
          ( \hd -> case _ of
              TcoExpr _ PrimUndefined ->
                build $ EsCall hd []
              arg ->
                build $ EsCall hd [ EsArrayValue $ codegenExpr env arg ]
          )
          (codegenExpr env a)
          bs
  Abs idents body -> do
    let result = freshNames RefStrict env idents
    esCurriedFunction (toEsIdent <$> NonEmptyArray.toArray result.value) (codegenBlockStatements pureMode result.accum body)
  RecAbs _ idents body -> do
    let result = freshNames RefStrict env idents
    esCurriedFunction (toEsIdent <$> NonEmptyArray.toArray result.value) (codegenBlockStatements pureMode result.accum body)
  UncurriedAbs idents body -> do
    let result = freshNames RefStrict env idents
    esArrowFunction (toEsIdent <$> result.value) (codegenBlockStatements pureMode result.accum body)
  UncurriedApp a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- inlineApp env qual (InlineApp bs) ->
            expr'
      _ ->
        build $ EsCall (codegenExpr env a) (EsArrayValue <<< codegenExpr env <$> bs)
  UncurriedEffectAbs idents body -> do
    let result = freshNames RefStrict env idents
    esArrowFunction (toEsIdent <$> result.value) (codegenBlockStatements effectMode result.accum body)
  UncurriedEffectApp a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- inlineApp env qual (InlineEffectApp bs) ->
            expr'
      _ ->
        codegenEffectBlock env tcoExpr
  Accessor a (GetProp prop) ->
    build $ EsAccess (codegenExpr env a) prop
  Accessor a (GetCtorField _ _ _ _ _ ix) ->
    build $ EsAccess (codegenExpr env a) ("_" <> show (ix + 1))
  Accessor a (GetIndex ix) ->
    build $ EsIndex (codegenExpr env a) (build (EsInt ix))
  Update a props ->
    build $ EsObject $ Array.cons (EsObjectSpread (codegenExpr env a)) $ codegenObjectElement env <$> props
  CtorDef ct ty tag [] ->
    codegenCtor env currentModule ct ty tag []
  CtorDef ct ty tag fields ->
    esCurriedFunction (toEsIdent <<< Ident <$> fields)
      [ build $ EsReturn $ Just $ codegenCtor env currentModule ct ty tag $
          (build <<< EsIdent <<< Qualified Nothing <<< toEsIdent <<< Ident) <$> fields
      ]
  CtorSaturated (Qualified qual _) ct ty tag fields ->
    codegenCtor env (fromMaybe currentModule qual) ct ty tag (codegenExpr env <<< snd <$> fields)
  PrimOp op ->
    codegenPrimOp env op
  PrimEffect _ ->
    codegenEffectBlock env tcoExpr
  PrimUndefined ->
    build EsUndefined
  Fail "Failed pattern match" ->
    build $ EsRuntime EsFail
  Fail _ ->
    unsafeCrashWith "Unsupported fail."
  Branch _ _ ->
    codegenPureBlock env tcoExpr
  LetRec _ _ _ ->
    codegenPureBlock env tcoExpr
  Let _ _ _ _ ->
    codegenPureBlock env tcoExpr
  RecLet _ _ _ _ _ ->
    codegenPureBlock env tcoExpr
  EffectBind _ _ _ _ ->
    codegenEffectBlock env tcoExpr
  EffectPure _ ->
    codegenEffectBlock env tcoExpr
  EffectDefer _ ->
    codegenEffectBlock env tcoExpr

codegenPureBlock :: CodegenEnv -> TcoExpr -> EsExpr
codegenPureBlock env a = build $ EsCall (esArrowFunction [] (codegenBlockStatements pureMode env a)) []

codegenEffectBlock :: CodegenEnv -> TcoExpr -> EsExpr
codegenEffectBlock env = esArrowFunction [] <<< codegenBlockStatements effectMode env

codegenBlockStatements :: BlockMode -> CodegenEnv -> TcoExpr -> Array EsExpr
codegenBlockStatements = go []
  where
  go acc mode env@(CodegenEnv { currentModule }) tcoExpr@(TcoExpr (TcoAnalysis analysis) expr) = case expr of
    LetRec lvl bindings body
      | Just tco <- toTcoBindings analysis.role bindings -> do
          let locals = flip Tuple lvl <<< Just <<< _.name <$> tco
          let tcoRefs = uncurry TcoLocal <$> locals
          let { value: tcoNames, accum: env' } = freshNames RefStrict env locals
          let
            Tuple tcoIdent env'' = case NonEmptyArray.toArray tcoNames of
              [ tcoIdent ] -> Tuple tcoIdent env'
              _ -> freshName RefStrict (Just (asTcoMutualIdent (_.name <$> tco))) lvl env'
          if isTcoJoin mode.tcoScope analysis.role then do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) mode
            let lines = codegenTcoMutualLoopBindings mode' env'' tcoIdent (NonEmptyArray.zip tcoNames tco)
            go (acc <> lines) (foldr pushTcoJoin mode locals) env'' body
          else do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) (noTco mode)
            let lines = codegenTcoMutualLoopBindings mode' env'' tcoIdent (NonEmptyArray.zip tcoNames tco)
            go (acc <> lines) mode env'' body
      | otherwise -> do
          let group = NonEmptyArray.toArray $ flip CodegenLocal lvl <<< Just <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding currentModule group) bindings
          let result1 = freshBindingGroup RefLazy lvl env lazyBindings.no
          let result2 = freshBindingGroup RefStrict lvl result1.accum lazyBindings.yes
          let
            lines = fold
              [ codegenBindings result2.accum result2.value
              , codegenLazyBindings result2.accum result1.value
              , codegenLazyInits $ fst <$> result1.value
              ]
          go (acc <> lines) mode (foldr strictCodegenRef result2.accum group) body
    Let ident lvl binding body
      | Just tco <- toTcoJoin mode.tcoScope analysis.role binding -> do
          let Tuple tcoIdent env' = freshName RefStrict ident lvl env
          let line = codegenTcoJoinBinding mode env tcoIdent tco
          go (Array.snoc acc line) (pushTcoJoin (Tuple ident lvl) mode) env' body
      -- -- HACK: This simplifies the case where, within an effect block, if a let
      -- -- binding to an effect is immediately invoked in a bind, it will get
      -- -- inlined. This doesn't happen in the usual semantics, but can arise
      -- -- through effect loop inlining. A less hacky solution would entail more
      -- -- analysis and simplification on the ES AST.
      | mode.effect
      , TcoExpr a1 (EffectBind ident2 lvl2 (TcoExpr a2 (Local ident3 lvl3)) next) <- body
      , ident == ident3 && lvl == lvl3
      , totalUsagesOf (TcoLocal ident lvl) (tcoAnalysisOf body) == 1 ->
          go acc mode env $ TcoExpr a1 (EffectBind ident2 lvl2 binding next)
      | otherwise -> do
          let Tuple ident' env' = freshName RefStrict ident lvl env
          let line = esBinding (toEsIdent ident') (codegenExpr env binding)
          go (Array.snoc acc line) mode env' body
    Branch bs def ->
      acc <> codegenBlockBranches mode env bs def
    EffectBind ident lvl (TcoExpr _ (PrimEffect (EffectRefNew val))) body
      | mode.effect && canUnboxRef (TcoLocal ident lvl) (tcoAnalysisOf body) -> do
          let Tuple ident' env' = freshName RefUnboxed ident lvl env
          let line = codegenUnboxedRefBinding env ident' val
          go (Array.snoc acc line) mode env' body
    EffectBind ident lvl eff body
      | mode.effect && totalUsagesOf (TcoLocal ident lvl) (tcoAnalysisOf body) == 0 ->
          case eff of
            TcoExpr _ (Branch bs def) -> do
              let lines = codegenBlockBranches (mode { return = Discard }) env bs def -- TODO
              go (acc <> lines) mode env body
            _ -> do
              let line = codegenBindEffect env eff
              go (Array.snoc acc line) mode env body
    EffectBind ident lvl eff body
      | mode.effect -> do
          let Tuple newIdent env' = freshName RefStrict ident lvl env
          let line = esBinding (toEsIdent newIdent) $ codegenBindEffect env eff
          go (Array.snoc acc line) mode env' body
    EffectPure expr'
      | mode.effect ->
          acc <> codegenBlockReturn (mode { effect = false }) env expr'
    EffectDefer expr'
      | mode.effect ->
          go acc mode env expr'
    App (TcoExpr _ (Local ident lvl)) bs
      | Just tco <- Tco.popTcoScope (TcoLocal ident lvl) mode.tcoScope ->
          acc <> codegenTcoJump mode tco (codegenExpr env <$> NonEmptyArray.toArray bs)
      | Set.member (Tuple ident lvl) mode.tcoJoins ->
          acc <> codegenTcoJoin mode (codegenExpr env tcoExpr)
    App (TcoExpr _ (Var qual)) bs
      | Just tco <- Tco.popTcoScope (TcoTopLevel qual) mode.tcoScope ->
          acc <> codegenTcoJump mode tco (codegenExpr env <$> NonEmptyArray.toArray bs)
    UncurriedApp (TcoExpr _ (Local ident lvl)) _
      | Set.member (Tuple ident lvl) mode.tcoJoins ->
          acc <> codegenTcoJoin mode (codegenExpr env tcoExpr)
    Fail _ ->
      Array.snoc acc (codegenExpr env tcoExpr)
    _ ->
      acc <> codegenBlockReturn mode env tcoExpr

codegenBindings :: CodegenEnv -> Array (Tuple Ident TcoExpr) -> Array EsExpr
codegenBindings env = map (uncurry esBinding <<< bimap toEsIdent (codegenExpr env))

codegenLazyBindings :: CodegenEnv -> Array (Tuple Ident TcoExpr) -> Array EsExpr
codegenLazyBindings env = map (uncurry esBinding <<< bimap asLazyIdent (esLazyBinding <<< codegenExpr env))

codegenLazyInits :: Array Ident -> Array EsExpr
codegenLazyInits = map \id -> esBinding (toEsIdent id) $ build $ EsCall (build (EsIdent (Qualified Nothing (asLazyIdent id)))) []

codegenBlockReturn :: BlockMode -> CodegenEnv -> TcoExpr -> Array EsExpr
codegenBlockReturn mode env tcoExpr
  | Just tco <- Tco.unwindTcoScope mode.tcoScope =
      codegenTcoReturn mode tco $ codegenExpr env tcoExpr
  | mode.effect = do
      let expr = codegenBindEffect env tcoExpr
      case mode.return of
        Continue ->
          [ expr
          , build $ EsContinue
          ]
        Discard ->
          pure expr
        Return ->
          pure $ build $ EsReturn $ Just expr
  | otherwise =
      case mode.return of
        Continue ->
          pure $ build $ EsContinue
        Discard ->
          []
        Return ->
          pure $ build $ EsReturn $ Just $ codegenExpr env tcoExpr

codegenBlockBranches :: BlockMode -> CodegenEnv -> NonEmptyArray (Pair TcoExpr) -> TcoExpr -> Array EsExpr
codegenBlockBranches mode env bs def = case mode.return of
  Discard ->
    foldr (\p -> pure <<< build <<< uncurry EsIfElse (go p)) (codegenBlockStatements mode env def) bs
  _ ->
    NonEmptyArray.toArray (build <<< flip (uncurry EsIfElse) [] <<< go <$> bs) <> codegenBlockStatements mode env def
  where
  go :: Pair TcoExpr -> Tuple EsExpr (Array EsExpr)
  go (Pair a b@(TcoExpr _ b')) = case b' of
    Branch next nextDef ->
      Tuple (codegenExpr env a) $ codegenBlockBranches mode env next nextDef
    _ ->
      Tuple (codegenExpr env a) $ codegenBlockStatements mode env b

codegenBindEffect :: CodegenEnv -> TcoExpr -> EsExpr
codegenBindEffect env tcoExpr@(TcoExpr _ expr) = case expr of
  PrimEffect a ->
    codegenPrimEffect env a
  Branch _ _ ->
    build $ EsCall (esArrowFunction [] (codegenBlockStatements effectMode env tcoExpr)) []
  UncurriedEffectApp a bs ->
    build $ EsCall (codegenExpr env a) (EsArrayValue <<< codegenExpr env <$> bs)
  _ ->
    build $ EsCall (codegenExpr env tcoExpr) []

codegenPrimEffect :: CodegenEnv -> BackendEffect TcoExpr -> EsExpr
codegenPrimEffect env@(CodegenEnv { names }) = case _ of
  EffectRefNew a ->
    build $ EsObject [ codegenObjectElement env $ Prop "value" a ]
  EffectRefRead a@(TcoExpr _ (Local ident lvl))
    | Just (Tuple _ RefUnboxed) <- Map.lookup (CodegenLocal ident lvl) names ->
        codegenExpr env a
  EffectRefRead a ->
    build $ EsAccess (codegenExpr env a) "value"
  EffectRefWrite a@(TcoExpr _ (Local ident lvl)) b
    | Just (Tuple _ RefUnboxed) <- Map.lookup (CodegenLocal ident lvl) names ->
        build $ EsAssign (codegenExpr env a) (codegenExpr env b)
  EffectRefWrite a b ->
    build $ EsAssign (build (EsAccess (codegenExpr env a) "value")) (codegenExpr env b)

codegenUnboxedRefBinding :: CodegenEnv -> Ident -> TcoExpr -> EsExpr
codegenUnboxedRefBinding env ident expr = build $ EsLet $ NonEmptyArray.singleton $ Tuple (toEsIdent ident) $ Just $ codegenExpr env expr

codegenTcoJoinBinding :: BlockMode -> CodegenEnv -> Ident -> TcoJoin -> EsExpr
codegenTcoJoinBinding mode env tcoIdent tco = do
  let result = freshNames RefStrict env tco.arguments
  let fn = if tco.curried then esCurriedFunction else esArrowFunction
  esBinding (toEsIdent tcoIdent) $ fn (toEsIdent <$> result.value) $ codegenBlockStatements (mode { tco = false }) result.accum tco.body

codegenTcoMutualLoopBindings :: BlockMode -> CodegenEnv -> Ident -> NonEmptyArray (Tuple Ident TcoBinding) -> Array EsExpr
codegenTcoMutualLoopBindings mode env tcoIdent bindings = case NonEmptyArray.toArray bindings of
  [ Tuple ident tco ] ->
    pure $ codegenTcoLoopBinding mode env ident tco
  bindings' -> do
    let maxArgs = maximum $ NonEmptyArray.length <<< _.arguments <<< snd <$> bindings
    let argIdents = flip asTcoArgIdent tcoIdent <$> NonEmptyArray.range 0 (maxArgs - 1)
    let branchIdent = asTcoBranchIdent tcoIdent
    pure $ build $ EsConst $ NonEmptyArray.cons'
      ( Tuple (EsBindingIdent (toEsIdent tcoIdent)) $ codegenMutualTcoFunction tcoIdent (NonEmptyArray.cons branchIdent argIdents)
          ( mapWithIndex
              ( \ix (Tuple _ tco) -> do
                  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
                  let cond = build $ EsBinary EsEquals (build (EsIdent (Qualified Nothing branchIdent))) (build (EsInt ix))
                  let head = build $ EsConst $ NonEmptyArray.zipWith (\arg var -> Tuple (EsBindingIdent (toEsIdent var)) $ build $ EsIdent $ Qualified Nothing $ toEsIdent arg) argIdents argNames
                  let body = codegenBlockStatements (mode { tco = true }) env' tco.body
                  build $ EsIfElse cond (Array.cons head body) []
              )
              bindings'
          )
      )
      ( mapWithIndex
          ( \ix (Tuple ident { arguments: args }) -> do
              let { value: idents } = freshNames RefStrict env args
              Tuple (EsBindingIdent (toEsIdent ident)) $ esCurriedFunction (toEsIdent <$> NonEmptyArray.toArray idents)
                [ build $ EsReturn $ Just $ build $ EsCall (build (EsIdent (Qualified Nothing (toEsIdent tcoIdent))))
                    $ Array.cons (EsArrayValue (build (EsInt ix))) (EsArrayValue <<< build <<< EsIdent <<< Qualified Nothing <<< toEsIdent <$> NonEmptyArray.toArray idents)
                ]
          )
          bindings'
      )

codegenTcoLoopBinding :: BlockMode -> CodegenEnv -> Ident -> TcoBinding -> EsExpr
codegenTcoLoopBinding mode env tcoIdent tco = do
  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
  let argIdents = mapWithIndex (Tuple <<< flip asTcoArgIdent tcoIdent) argNames
  esBinding (toEsIdent tcoIdent) $ codegenTcoFunction tcoIdent (fst <$> argIdents) $ fold
    [ pure $ build $ EsConst $ (\(Tuple arg var) -> Tuple (EsBindingIdent (toEsIdent var)) $ build $ EsIdent $ Qualified Nothing arg) <$> argIdents
    , codegenBlockStatements (mode { tco = true }) env' tco.body
    ]

codegenMutualTcoFunction :: Ident -> NonEmptyArray EsIdent -> Array EsExpr -> EsExpr
codegenMutualTcoFunction tcoIdent args body = esArrowFunction (asTcoCopyIdent <$> NonEmptyArray.toArray args) $ codegenTcoFunctionBody tcoIdent args body

codegenTcoFunction :: Ident -> NonEmptyArray EsIdent -> Array EsExpr -> EsExpr
codegenTcoFunction tcoIdent args body = esCurriedFunction (NonEmptyArray.toArray (asTcoCopyIdent <$> args)) $ codegenTcoFunctionBody tcoIdent args body

codegenTcoFunctionBody :: Ident -> NonEmptyArray EsIdent -> Array EsExpr -> Array EsExpr
codegenTcoFunctionBody tcoIdent args body =
  [ build $ EsLet $ NonEmptyArray.appendArray bindings
      [ Tuple (asTcoLoopIdent tcoIdent) $ Just $ build $ EsBoolean true
      , Tuple (asTcoReturnIdent tcoIdent) Nothing
      ]
  , build $ EsWhile (build (EsIdent (Qualified Nothing (asTcoLoopIdent tcoIdent)))) body
  , build $ EsReturn $ Just $ build $ EsIdent (Qualified Nothing (asTcoReturnIdent tcoIdent))
  ]
  where
  bindings = (\arg -> Tuple (toEsIdent arg) $ Just $ build $ EsIdent $ Qualified Nothing $ asTcoCopyIdent arg) <$> args

codegenTcoApp :: TcoPop -> Array EsExpr -> Array EsExpr
codegenTcoApp pop args = fold
  [ Monoid.guard (NonEmptyArray.length pop.group > 1)
      [ esAssignIdent (asTcoBranchIdent pop.ident) $ build $ EsInt pop.index ]
  , mapWithIndex (esAssignIdent <<< flip asTcoArgIdent pop.ident) args
  ]

codegenTcoReturn :: BlockMode -> Tuple Ident (List Ident) -> EsExpr -> Array EsExpr
codegenTcoReturn mode (Tuple tcoIdent stk) expr =
  [ foldr (esAssignIdent <<< asTcoLoopIdent) (build (EsBoolean false)) $ List.Cons tcoIdent stk
  , esAssignIdent (asTcoReturnIdent tcoIdent) expr
  , if mode.tco then build EsContinue else build $ EsReturn Nothing
  ]

codegenTcoJump :: BlockMode -> TcoPop -> Array EsExpr -> Array EsExpr
codegenTcoJump mode pop args =
  stkStatements
    <> codegenTcoApp pop args
    <> [ if mode.tco then build EsContinue else build $ EsReturn Nothing ]
  where
  stkStatements
    | List.null pop.stack = []
    | otherwise =
        pure $ foldr (esAssignIdent <<< asTcoLoopIdent) (build (EsBoolean false)) pop.stack

codegenTcoJoin :: BlockMode -> EsExpr -> Array EsExpr
codegenTcoJoin mode expr =
  [ expr
  , if mode.tco then
      build EsContinue
    else
      build $ EsReturn Nothing
  ]

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
    EsExpr _ (EsIdent (Qualified Nothing p2))
      | printIdentString p2 == p1 ->
          EsObjectPun p2
    other ->
      EsObjectField p1 other

codegenCtor :: CodegenEnv -> ModuleName -> ConstructorType -> ProperName -> Ident -> Array EsExpr -> EsExpr
codegenCtor env@(CodegenEnv { currentModule, options }) mod ct name tag values = case ct of
  SumType -> do
    let { ctorMeta } = lookupCtorInfo env (Qualified (Just mod) tag)
    build $ EsCall ctorName $ Array.cons (EsArrayValue (codegenTag options tag ctorMeta)) ctorValues
  ProductType ->
    build $ EsCall ctorName ctorValues
  where
  ctorName = build $ EsIdent $ Qualified ctorModule $ asCtorIdent name
  ctorModule = if mod == currentModule then Nothing else Just mod
  ctorValues = EsArrayValue <$> values

codegenTag :: CodegenOptions -> Ident -> CtorMeta -> EsExpr
codegenTag opts (Ident ctor) { tag }
  | opts.intTags =
      build $ EsCommentTrailing (build (EsInt tag)) ctor
  | otherwise =
      build $ EsString ctor

codegenName :: CodegenName -> EsExpr
codegenName (Tuple ident refType) = case refType of
  RefStrict ->
    build $ EsIdent $ Qualified Nothing $ toEsIdent ident
  RefUnboxed ->
    build $ EsIdent $ Qualified Nothing $ toEsIdent ident
  RefLazy ->
    build $ EsCall (build (EsIdent (Qualified Nothing (asLazyIdent ident)))) []

codegenPrimOp :: CodegenEnv -> BackendOperator TcoExpr -> EsExpr
codegenPrimOp env@(CodegenEnv { options }) = case _ of
  Op1 op a -> do
    let expr = codegenExpr env a
    case op of
      OpBooleanNot ->
        build $ EsUnary EsNot expr
      OpIntBitNot ->
        build $ EsUnary EsBitNegate expr
      OpIntNegate ->
        build $ EsUnary EsNegate expr
      OpNumberNegate ->
        build $ EsUnary EsNegate expr
      OpArrayLength ->
        build $ EsAccess expr "length"
      OpIsTag qual@(Qualified _ tag) ->
        case lookupCtorInfo env qual of
          { size, ctorMeta }
            | size == 0 ->
                build $ EsBinary EsEquals expr $ codegenTag options tag ctorMeta
            | otherwise ->
                build $ EsBinary EsEquals (build (EsAccess expr "tag")) $ codegenTag options tag ctorMeta
  Op2 op a b -> do
    let expr1 = codegenExpr env a
    let expr2 = codegenExpr env b
    case op of
      OpArrayIndex ->
        build $ EsIndex expr1 expr2
      OpBooleanAnd ->
        build $ EsBinary EsAnd expr1 expr2
      OpBooleanOr ->
        build $ EsBinary EsOr expr1 expr2
      OpBooleanOrd ord ->
        build $ EsBinary (ordOp ord) expr1 expr2
      OpCharOrd ord ->
        build $ EsBinary (ordOp ord) expr1 expr2
      OpIntBitAnd ->
        build $ EsBinary EsBitAnd expr1 expr2
      OpIntBitOr ->
        build $ EsBinary EsBitOr expr1 expr2
      OpIntBitShiftLeft ->
        build $ EsBinary EsBitShiftLeft expr1 expr2
      OpIntBitShiftRight ->
        build $ EsBinary EsBitShitRight expr1 expr2
      OpIntBitXor ->
        build $ EsBinary EsBitXor expr1 expr2
      OpIntBitZeroFillShiftRight ->
        build $ EsBinary EsZeroFillShiftRight expr1 expr2
      OpIntNum num ->
        build $ EsBinary EsBitOr (build (EsBinary (numOp num) expr1 expr2)) (build (EsInt 0))
      OpIntOrd ord ->
        build $ EsBinary (ordOp ord) expr1 expr2
      OpNumberNum num ->
        build $ EsBinary (numOp num) expr1 expr2
      OpNumberOrd ord ->
        build $ EsBinary (ordOp ord) expr1 expr2
      OpStringAppend ->
        build $ EsBinary EsAdd expr1 expr2
      OpStringOrd ord ->
        build $ EsBinary (ordOp ord) expr1 expr2
  where
  ordOp = case _ of
    OpEq -> EsEquals
    OpNotEq -> EsNotEquals
    OpGt -> EsGreaterThan
    OpGte -> EsGreaterThanEqual
    OpLt -> EsLessThan
    OpLte -> EsLessThanEqual

  numOp = case _ of
    OpAdd -> EsAdd
    OpDivide -> EsDivide
    OpMultiply -> EsMultiply
    OpSubtract -> EsSubtract

codegenCtorForType :: CodegenOptions -> ProperName -> DataTypeMeta -> EsExpr
codegenCtorForType opts name meta = do
  let
    fieldArgs
      | meta.size > 0 =
          Generated <<< append "_" <<< show <$> Array.range 1 meta.size
      | otherwise =
          []
  case Map.toUnfoldable meta.constructors of
    [ Tuple ctor ctorMeta ] -> do
      -- Only add the tag for product types if we care what the name is,
      -- otherwise they are all 0 and it might as well not be there.
      let
        args
          | opts.intTags =
              EsObjectPun <$> fieldArgs
          | otherwise =
              Array.cons (EsObjectField "tag" (codegenTag opts ctor ctorMeta)) $ EsObjectPun <$> fieldArgs
      esBinding (asCtorIdent name) $ esArrowFunction fieldArgs
        [ build $ EsReturn $ Just $ build $ EsObject args ]
    _ -> do
      let args = Array.cons (Generated "tag") fieldArgs
      let
        value
          | meta.size == 0 =
              EsIdent $ Qualified Nothing (Generated "tag")
          | otherwise =
              EsObject $ EsObjectPun <$> args
      esBinding (asCtorIdent name) $ esArrowFunction args
        [ build $ EsReturn $ Just $ build value ]

asTcoLoopIdent :: forall a. ToEsIdent a => a -> EsIdent
asTcoLoopIdent = toEsIdentWith "c"

asTcoReturnIdent :: forall a. ToEsIdent a => a -> EsIdent
asTcoReturnIdent = toEsIdentWith "r"

asTcoBranchIdent :: forall a. ToEsIdent a => a -> EsIdent
asTcoBranchIdent = toEsIdentWith "b"

asTcoArgIdent :: forall a. ToEsIdent a => Int -> a -> EsIdent
asTcoArgIdent ix = toEsIdentWith ("a" <> show ix)

asTcoCopyIdent :: forall a. ToEsIdent a => a -> EsIdent
asTcoCopyIdent = toEsIdentWith "copy"

-- TODO
asTcoMutualIdent :: NonEmptyArray Ident -> Ident
asTcoMutualIdent idents = case NonEmptyArray.toArray idents of
  [ ident ] -> ident
  _ -> Ident $ "$" <> foldMap (esEscapeIdent <<< String.take 5 <<< unwrap) idents

asLazyIdent :: forall a. ToEsIdent a => a -> EsIdent
asLazyIdent = toEsIdentWith "lazy"

asCtorIdent :: ProperName -> EsIdent
asCtorIdent (ProperName name) = Generated ("$" <> esEscapeIdent name)

esLocalIdent :: Maybe Ident -> Level -> Ident
esLocalIdent mb (Level lvl) = case mb of
  Just (Ident a) ->
    Ident (a <> "$" <> show lvl)
  Nothing ->
    Ident ("$" <> show lvl)

isLazyBinding :: ModuleName -> Array CodegenRef -> Tuple Ident TcoExpr -> Boolean
isLazyBinding currentModule group (Tuple _ tcoExpr) = go tcoExpr
  where
  -- TODO: Should this be fused with the TCO pass?
  go (TcoExpr _ expr) = case expr of
    Abs _ _ ->
      true
    RecAbs _ _ _ ->
      true
    UncurriedAbs _ _ ->
      true
    UncurriedEffectAbs _ _ ->
      true
    CtorDef _ _ _ _ ->
      true
    EffectBind _ _ _ _ ->
      true
    EffectPure _ ->
      true
    EffectDefer _ ->
      true
    Var (Qualified (Just mn) ident) | mn == currentModule ->
      not $ Array.elem (CodegenTopLevel ident) group
    Var _ ->
      true
    Local ident lvl ->
      not $ Array.elem (CodegenLocal ident lvl) group
    Lit lit ->
      all go lit
    Accessor a _ ->
      go a
    Update a b ->
      go a && (all (go <<< propValue)) b
    CtorSaturated _ _ _ _ vals ->
      all (go <<< snd) vals
    PrimOp op ->
      all go op
    Fail _ ->
      false
    PrimEffect _ ->
      false
    PrimUndefined ->
      false
    LetRec _ _ _ ->
      false
    Let _ _ _ _ ->
      false
    RecLet _ _ _ _ _ ->
      false
    Branch _ _ ->
      false
    App _ _ ->
      false
    UncurriedApp _ _ ->
      false
    UncurriedEffectApp _ _ ->
      false

lookupCtorInfo :: CodegenEnv -> Qualified Ident -> { ctorMeta :: CtorMeta, size :: Int }
lookupCtorInfo (CodegenEnv env) qual = case Map.lookup qual env.implementations of
  Just (Tuple _ (ExternCtor dm@{ size } _ _ tag _))
    | Just ctorMeta <- Map.lookup tag dm.constructors ->
        { ctorMeta, size }
  _ ->
    unsafeCrashWith $ "Constructor meta not found: "
      <> foldMap unwrap (qualifiedModuleName qual)
      <> "."
      <> unwrap (unQualified qual)

totalUsagesOf :: TcoRef -> TcoAnalysis -> Int
totalUsagesOf ref (TcoAnalysis { usages }) = case Map.lookup ref usages of
  Just (TcoUsage { total }) ->
    total
  _ ->
    0

canUnboxRef :: TcoRef -> TcoAnalysis -> Boolean
canUnboxRef ref (TcoAnalysis { usages }) = case Map.lookup ref usages of
  Just (TcoUsage { total, readWrite }) ->
    total == readWrite
  Nothing ->
    false
