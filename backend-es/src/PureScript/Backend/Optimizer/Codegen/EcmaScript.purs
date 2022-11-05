module PureScript.Backend.Optimizer.Codegen.EcmaScript where

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
import Data.Map (Map, SemigroupMap(..))
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
import Dodo as Dodo
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Optimizer.Analysis (Usage(..))
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Common (esComment, esEscapeIdent)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Inline (esInlineMap)
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax (class ToEsIdent, EsAnalysis(..), EsArrayElement(..), EsBinaryOp(..), EsExpr(..), EsIdent(..), EsModuleStatement(..), EsObjectElement(..), EsRuntimeOp(..), EsSyntax(..), EsUnaryOp(..), build, defaultPrintOptions, esAnalysisOf, esArrowFunction, esAssignIdent, esBinding, esCurriedFunction, esLazyBinding, printIdentString, printModuleStatement, toEsIdent, toEsIdentWith)
import PureScript.Backend.Optimizer.Codegen.Tco (LocalRef, TcoAnalysis(..), TcoExpr(..), TcoPop, TcoRef(..), TcoRole, TcoScope, TcoScopeItem)
import PureScript.Backend.Optimizer.Codegen.Tco as Tco
import PureScript.Backend.Optimizer.Convert (BackendBindingGroup, BackendImplementations, BackendModule)
import PureScript.Backend.Optimizer.CoreFn (ConstructorType(..), Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..), propValue, qualifiedModuleName, unQualified)
import PureScript.Backend.Optimizer.Semantics (CtorMeta, DataTypeMeta, ExternImpl(..), NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

data CodegenRefType = RefStrict | RefLazy

type CodegenName = Tuple Ident CodegenRefType

data CodegenRef
  = CodegenLocal Ident Level
  | CodegenTopLevel Ident

derive instance Eq CodegenRef
derive instance Ord CodegenRef

type CodegenOptions =
  { intTags :: Boolean
  }

type CodegenEnv =
  { currentModule :: ModuleName
  , bound :: Map Ident Int
  , names :: Map CodegenRef CodegenName
  , emitPure :: Boolean
  , options :: CodegenOptions
  , implementations :: BackendImplementations
  }

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

type BlockMode =
  { effect :: Boolean
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
boundTopLevel ident env = env { bound = Map.insert ident 1 env.bound }

freshName :: CodegenRefType -> Maybe Ident -> Level -> CodegenEnv -> Tuple Ident CodegenEnv
freshName refType ident lvl env = case ident of
  Nothing ->
    Tuple (Ident ("$" <> show (unwrap lvl))) env
  Just id ->
    case Map.lookup id env.bound of
      Nothing ->
        Tuple id $ env
          { bound = Map.insert id 1 env.bound
          , names = Map.insert (CodegenLocal id lvl) (Tuple id refType) env.names
          }
      Just n -> do
        let fresh = Ident (unwrap id <> "$" <> show n)
        Tuple fresh $ env
          { bound = Map.insert id (n + 1) env.bound
          , names = Map.insert (CodegenLocal id lvl) (Tuple fresh refType) env.names
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
lazyTopLevel ident env = env { names = Map.insert (CodegenTopLevel ident) (Tuple ident RefLazy) env.names }

strictCodegenRef :: CodegenRef -> CodegenEnv -> CodegenEnv
strictCodegenRef ref env = env { names = Map.update (Just <<< (RefStrict <$ _)) ref env.names }

renameLocal :: Maybe Ident -> Level -> CodegenEnv -> CodegenName
renameLocal ident lvl env =
  case ident >>= \id -> Map.lookup (CodegenLocal id lvl) env.names of
    Nothing ->
      Tuple (esLocalIdent ident lvl) RefStrict
    Just id ->
      id

renameTopLevel :: Ident -> CodegenEnv -> CodegenName
renameTopLevel ident env = fromMaybe (Tuple ident RefStrict) $ Map.lookup (CodegenTopLevel ident) env.names

pureMode :: BlockMode
pureMode = { effect: false, tco: false, tcoScope: List.Nil, tcoJoins: Set.empty }

effectMode :: BlockMode
effectMode = pureMode { effect = true }

pushTcoScope :: TcoScopeItem -> BlockMode -> BlockMode
pushTcoScope scopeItem mode = mode { tco = true, tcoScope = List.Cons scopeItem mode.tcoScope }

pushTcoJoin :: LocalRef -> BlockMode -> BlockMode
pushTcoJoin ref mode = mode { tcoJoins = Set.insert ref mode.tcoJoins }

noTco :: BlockMode -> BlockMode
noTco = _ { tco = false, tcoScope = List.Nil, tcoJoins = Set.empty }

codegenModule :: forall a. CodegenOptions -> BackendImplementations -> BackendModule -> Dodo.Doc a
codegenModule options implementations mod = do
  let
    topLevelBound :: Map Ident Int
    topLevelBound = foldl
      ( \bs { bindings } ->
          foldr (flip Map.insert 1 <<< fst) bs bindings
      )
      (foldr (flip Map.insert 1) Map.empty mod.foreign)
      mod.bindings

    codegenEnv :: CodegenEnv
    codegenEnv =
      { currentModule: mod.name
      , bound: topLevelBound
      , names: Map.empty
      , emitPure: true
      , options
      , implementations
      }

    dataTypes :: Array (Tuple ProperName DataTypeMeta)
    dataTypes = Map.toUnfoldable mod.dataTypes

    bindingExports :: SemigroupMap (Maybe String) (Array EsIdent)
    bindingExports = SemigroupMap $ Map.singleton Nothing $ map (toEsIdent <<< fst) <<< _.bindings =<< mod.bindings

    dataTypeExports :: SemigroupMap (Maybe String) (Array EsIdent)
    dataTypeExports = SemigroupMap $ Map.singleton Nothing $ asCtorIdent <<< fst <$> dataTypes

    exportsByPath :: SemigroupMap (Maybe String) (Array EsIdent)
    exportsByPath = dataTypeExports <> bindingExports

    foreignImports :: Array EsIdent
    foreignImports = toEsIdent <$> Set.toUnfoldable mod.foreign

    modBindings :: Array EsExpr
    modBindings = codegenTopLevelBindingGroup codegenEnv =<< mod.bindings

    modDeps :: Array ModuleName
    modDeps = Set.toUnfoldable (foldMap (_.deps <<< unwrap <<< esAnalysisOf) modBindings)

    EsAnalysis s = foldMap esAnalysisOf modBindings

    modStatements :: Dodo.Doc a
    modStatements = Dodo.lines $ map (printModuleStatement defaultPrintOptions) $ fold
      [ Monoid.guard s.runtime [ EsImportAllAs (Generated "$runtime") "../runtime.js" ]
      , (\mn -> EsImportAllAs (toEsIdent mn) (esModulePath mn)) <$> modDeps
      , Monoid.guard (not (Array.null foreignImports)) [ EsImport foreignImports (esForeignModulePath mod.name) ]
      , EsStatement <<< uncurry (codegenCtorForType codegenEnv) <$> dataTypes
      , EsStatement <$> modBindings
      , (\(Tuple p es) -> EsExport es p) <$> Map.toUnfoldable (unwrap exportsByPath)
      , Monoid.guard (not (Set.isEmpty mod.foreign)) [ EsExportAllFrom (esForeignModulePath mod.name) ]
      ]

  Monoid.guard (not (Array.null mod.comments))
    ( Dodo.lines (esComment <$> mod.comments)
        <> Dodo.break
    )
    <> modStatements
    <> Dodo.break

codegenTopLevelBindingGroup :: CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> Array EsExpr
codegenTopLevelBindingGroup env { bindings, recursive }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings = do
      let tcoGroup = Tco.topLevelTcoEnvGroup env.currentModule bindings'
      let bindings'' = map (Tco.analyze tcoGroup) <$> bindings'
      let tcoRefBindings = Tco.topLevelTcoRefBindings env.currentModule bindings''
      let isLoop = maybe false Tco.tcoRoleIsLoop tcoRefBindings
      case toTcoBindings { isLoop, joins: [] } bindings'' of
        Just tco -> do
          let tcoNames = _.name <$> tco
          let tcoIdent = asTcoMutualIdent tcoNames
          let tcoRefs = Tuple tcoIdent $ TcoTopLevel <<< Qualified (Just env.currentModule) <$> tcoNames
          let mode = pushTcoScope tcoRefs pureMode
          let env' = foldr boundTopLevel env tcoNames
          codegenTcoMutualLoopBindings mode (boundTopLevel tcoIdent env') tcoIdent (NonEmptyArray.zip tcoNames tco)
        Nothing -> do
          let group = CodegenTopLevel <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding env.currentModule group) bindings''
          let env' = foldr (lazyTopLevel <<< fst) env lazyBindings.no
          fold
            [ codegenBindings env' lazyBindings.yes
            , codegenLazyBindings env' lazyBindings.no
            , codegenLazyInits $ fst <$> lazyBindings.no
            ]
  | otherwise =
      codegenBindings env $ map (Tco.analyze []) <$> bindings

codegenExpr :: CodegenEnv -> TcoExpr -> EsExpr
codegenExpr env tcoExpr@(TcoExpr _ expr) = case expr of
  Var (Qualified (Just mn) ident) | mn == env.currentModule ->
    codegenName $ renameTopLevel ident env
  Var qual ->
    build $ EsIdent $ toEsIdent <$> qual
  Local ident lvl ->
    codegenName (renameLocal ident lvl env)
  Lit lit ->
    codegenLit env lit
  App a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- shouldInlineApp env qual (NonEmptyArray.toArray bs) ->
            expr'
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
    esCurriedFunction (toEsIdent <$> NonEmptyArray.toArray result.value) (codegenBlockStatements pureMode result.accum body)
  UncurriedAbs idents body -> do
    let result = freshNames RefStrict env idents
    esArrowFunction (toEsIdent <$> result.value) (codegenBlockStatements pureMode result.accum body)
  UncurriedApp a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- shouldInlineApp env qual bs ->
            expr'
      _ ->
        build $ EsCall (codegenExpr env a) (codegenExpr env <$> bs)
  UncurriedEffectAbs idents body -> do
    let result = freshNames RefStrict env idents
    esArrowFunction (toEsIdent <$> result.value) (codegenBlockStatements effectMode result.accum body)
  UncurriedEffectApp _ _ ->
    codegenEffectBlock env tcoExpr
  Accessor a (GetProp prop) ->
    build $ EsAccess (codegenExpr env a) prop
  Accessor a (GetOffset ix) ->
    build $ EsAccess (codegenExpr env a) ("_" <> show (ix + 1))
  Accessor a (GetIndex ix) ->
    build $ EsIndex (codegenExpr env a) (build (EsInt ix))
  Update a props ->
    build $ EsObject $ Array.cons (EsObjectSpread (codegenExpr env a)) $ codegenObjectElement env <$> props
  CtorDef ct ty tag [] ->
    codegenCtor env env.currentModule ct ty tag []
  CtorDef ct ty tag fields ->
    esCurriedFunction (toEsIdent <<< Ident <$> fields)
      [ build $ EsReturn $ Just $ codegenCtor env env.currentModule ct ty tag $
          (build <<< EsIdent <<< Qualified Nothing <<< toEsIdent <<< Ident) <$> fields
      ]
  CtorSaturated (Qualified qual _) ct ty tag fields ->
    codegenCtor env (fromMaybe env.currentModule qual) ct ty tag (codegenExpr env <<< snd <$> fields)
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
  EffectBind _ _ _ _ ->
    codegenEffectBlock env tcoExpr
  EffectPure _ ->
    codegenEffectBlock env tcoExpr

codegenPureBlock :: CodegenEnv -> TcoExpr -> EsExpr
codegenPureBlock env a = build $ EsCall (esArrowFunction [] (codegenBlockStatements pureMode env a)) []

codegenEffectBlock :: CodegenEnv -> TcoExpr -> EsExpr
codegenEffectBlock env = esArrowFunction [] <<< codegenBlockStatements effectMode env

codegenBlockStatements :: BlockMode -> CodegenEnv -> TcoExpr -> Array EsExpr
codegenBlockStatements = go []
  where
  go acc mode env tcoExpr@(TcoExpr (TcoAnalysis analysis) expr) = case expr of
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
          let group = NonEmptyArray.toArray $ flip CodegenLocal lvl <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding env.currentModule group) bindings
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
      | otherwise -> do
          let Tuple ident' env' = freshName RefStrict ident lvl env
          let line = esBinding (toEsIdent ident') (codegenExpr env binding)
          go (Array.snoc acc line) mode env' body
    Branch bs def ->
      acc <> codegenBlockBranches mode env bs def
    UncurriedEffectApp a bs | mode.effect -> do
      let line = build $ EsCall (codegenExpr env a) (codegenExpr env <$> bs)
      Array.snoc acc $ build $ EsReturn $ Just line
    EffectBind ident lvl eff body@(TcoExpr (TcoAnalysis { usages }) _) | mode.effect -> do
      let binding = codegenBindEffect env eff
      case Map.lookup (TcoLocal ident lvl) usages of
        Just (Usage { total }) | total > 0 -> do
          let Tuple newIdent env' = freshName RefStrict ident lvl env
          let line = esBinding (toEsIdent newIdent) binding
          go (Array.snoc acc line) mode env' body
        _ ->
          go (Array.snoc acc binding) mode env body
    EffectPure expr' | mode.effect ->
      acc <> codegenBlockReturn (mode { effect = false }) env expr'
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
codegenBlockReturn mode env tcoExpr@(TcoExpr _ expr)
  | Just tco <- Tco.unwindTcoScope mode.tcoScope =
      codegenTcoReturn mode tco (codegenExpr env tcoExpr)
  | mode.effect = case expr of
      PrimEffect eff ->
        pure $ build $ EsReturn $ Just $ codegenPrimEffect env eff
      _ ->
        pure $ build $ EsReturn $ Just $ build $ EsCall (codegenExpr env tcoExpr) []
  | otherwise =
      pure $ build $ EsReturn $ Just $ codegenExpr env tcoExpr

codegenBlockBranches :: BlockMode -> CodegenEnv -> NonEmptyArray (Pair TcoExpr) -> Maybe TcoExpr -> Array EsExpr
codegenBlockBranches mode env bs def =
  NonEmptyArray.toArray (build <<< flip (uncurry EsIfElse) [] <<< go <$> bs)
    <> maybe [] (codegenBlockStatements mode env) def
  where
  go :: Pair TcoExpr -> Tuple EsExpr (Array EsExpr)
  go (Pair a b@(TcoExpr _ b')) = case b' of
    Branch next nextDef ->
      Tuple (codegenExpr env a) $ codegenBlockBranches mode env next nextDef
    _ ->
      Tuple (codegenExpr env a) $ codegenBlockStatements mode env b

codegenBindEffect :: CodegenEnv -> TcoExpr -> EsExpr
codegenBindEffect env expr@(TcoExpr _ expr') = case expr' of
  PrimEffect a ->
    codegenPrimEffect env a
  UncurriedEffectApp a bs ->
    build $ EsCall (codegenExpr env a) (codegenExpr env <$> bs)
  _ ->
    build $ EsCall (codegenExpr env expr) []

codegenPrimEffect :: CodegenEnv -> BackendEffect TcoExpr -> EsExpr
codegenPrimEffect env = case _ of
  EffectRefNew a ->
    build $ EsObject [ codegenObjectElement env $ Prop "value" a ]
  EffectRefRead a ->
    build $ EsAccess (codegenExpr env a) "value"
  EffectRefWrite a b ->
    build $ EsAssign (build (EsAccess (codegenExpr env a) "value")) (codegenExpr env b)

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
      ( Tuple (toEsIdent tcoIdent) $ codegenMutualTcoFunction tcoIdent (NonEmptyArray.cons branchIdent argIdents)
          ( mapWithIndex
              ( \ix (Tuple _ tco) -> do
                  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
                  let cond = build $ EsBinary EsEquals (build (EsIdent (Qualified Nothing branchIdent))) (build (EsInt ix))
                  let head = build $ EsConst $ NonEmptyArray.zipWith (\arg var -> Tuple (toEsIdent var) $ build $ EsIdent $ Qualified Nothing $ toEsIdent arg) argIdents argNames
                  let body = codegenBlockStatements (mode { tco = true }) env' tco.body
                  build $ EsIfElse cond (Array.cons head body) []
              )
              bindings'
          )
      )
      ( mapWithIndex
          ( \ix (Tuple ident { arguments: args }) -> do
              let { value: idents } = freshNames RefStrict env args
              Tuple (toEsIdent ident) $ esCurriedFunction (toEsIdent <$> NonEmptyArray.toArray idents)
                [ build $ EsReturn $ Just $ build $ EsCall (build (EsIdent (Qualified Nothing (toEsIdent tcoIdent))))
                    $ Array.cons (build (EsInt ix)) (build <<< EsIdent <<< Qualified Nothing <<< toEsIdent <$> NonEmptyArray.toArray idents)
                ]
          )
          bindings'
      )

codegenTcoLoopBinding :: BlockMode -> CodegenEnv -> Ident -> TcoBinding -> EsExpr
codegenTcoLoopBinding mode env tcoIdent tco = do
  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
  let argIdents = mapWithIndex (Tuple <<< flip asTcoArgIdent tcoIdent) argNames
  esBinding (toEsIdent tcoIdent) $ codegenTcoFunction tcoIdent (fst <$> argIdents) $ fold
    [ pure $ build $ EsConst $ (\(Tuple arg var) -> Tuple (toEsIdent var) $ build $ EsIdent $ Qualified Nothing arg) <$> argIdents
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
codegenCtor env mod ct name tag values = case ct of
  SumType -> do
    let ctorMeta = lookupCtorMeta env (Qualified (Just mod) tag)
    build $ EsCall ctorName $ Array.cons (codegenTag env tag ctorMeta) values
  ProductType ->
    build $ EsCall ctorName values
  where
  ctorName = build $ EsIdent $ Qualified ctorModule $ asCtorIdent name
  ctorModule = if mod == env.currentModule then Nothing else Just mod

codegenTag :: CodegenEnv -> Ident -> CtorMeta -> EsExpr
codegenTag env (Ident ctor) { tag }
  | env.options.intTags =
      build $ EsCommentTrailing (build (EsInt tag)) ctor
  | otherwise =
      build $ EsString ctor

codegenName :: CodegenName -> EsExpr
codegenName (Tuple ident refType) = case refType of
  RefStrict ->
    build $ EsIdent $ Qualified Nothing $ toEsIdent ident
  RefLazy ->
    build $ EsCall (build (EsIdent (Qualified Nothing (asLazyIdent ident)))) []

codegenPrimOp :: CodegenEnv -> BackendOperator TcoExpr -> EsExpr
codegenPrimOp env = case _ of
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
        build $ EsBinary EsEquals (build (EsAccess expr "tag")) $ codegenTag env tag (lookupCtorMeta env qual)
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

codegenCtorForType :: CodegenEnv -> ProperName -> DataTypeMeta -> EsExpr
codegenCtorForType env name meta = do
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
          | env.options.intTags =
              EsObjectPun <$> fieldArgs
          | otherwise =
              Array.cons (EsObjectField "tag" (codegenTag env ctor ctorMeta)) $ EsObjectPun <$> fieldArgs
      esBinding (asCtorIdent name) $ esArrowFunction fieldArgs
        [ build $ EsReturn $ Just $ build $ EsObject args ]
    _ -> do
      let args = Array.cons (Generated "tag") fieldArgs
      esBinding (asCtorIdent name) $ esArrowFunction args
        [ build $ EsReturn $ Just $ build $ EsObject $ EsObjectPun <$> args ]

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

esModulePath :: ModuleName -> String
esModulePath (ModuleName mn) = "../" <> mn <> "/index.js"

esForeignModulePath :: ModuleName -> String
esForeignModulePath (ModuleName _) = "./foreign.js"

isLazyBinding :: ModuleName -> Array CodegenRef -> Tuple Ident TcoExpr -> Boolean
isLazyBinding currentModule group (Tuple _ tcoExpr) = go tcoExpr
  where
  -- TODO: Should this be fused with the TCO pass?
  go (TcoExpr _ expr) = case expr of
    Abs _ _ ->
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
    Var (Qualified (Just mn) ident) | mn == currentModule ->
      not $ Array.elem (CodegenTopLevel ident) group
    Var _ ->
      true
    Local (Just ident) lvl ->
      not $ Array.elem (CodegenLocal ident lvl) group
    Local _ _ ->
      true
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
    Branch _ _ ->
      false
    App _ _ ->
      false
    UncurriedApp _ _ ->
      false
    UncurriedEffectApp _ _ ->
      false

lookupCtorMeta :: CodegenEnv -> Qualified Ident -> CtorMeta
lookupCtorMeta env qual = case Map.lookup qual env.implementations of
  Just (Tuple _ (ExternCtor dm _ _ tag _))
    | Just meta <- Map.lookup tag dm.constructors ->
        meta
  _ ->
    unsafeCrashWith $ "Constructor meta not found: "
      <> foldMap unwrap (qualifiedModuleName qual)
      <> "."
      <> unwrap (unQualified qual)

shouldInlineApp :: CodegenEnv -> Qualified Ident -> Array TcoExpr -> Maybe EsExpr
shouldInlineApp env qual args = do
  fn <- Map.lookup qual esInlineMap
  fn (codegenExpr env) qual args
