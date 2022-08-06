module PureScript.Backend.Codegen.EcmaScript where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (all, any, fold, foldMap, foldl, foldr, maximum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (class Traversable, Accum, mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Analysis (Usage(..))
import PureScript.Backend.Codegen.EcmaScript.Common (EsStatement(..), esAccessor, esApp, esArray, esAssign, esAssignRef, esBinding, esBlock, esBlockStatements, esBranches, esChar, esComment, esContinue, esCurriedFn, esEffectBlock, esError, esEscapeSpecial, esExportAllFrom, esExports, esFn, esFwdRef, esIdent, esImport, esIndex, esInt, esLetBinding, esModuleName, esNumber, esOffset, esProp, esPure, esRecord, esReturn, esSepStatements, esString, esUndefined, esUpdate)
import PureScript.Backend.Codegen.EcmaScript.Inline (esInlineMap)
import PureScript.Backend.Codegen.Tco (LocalRef, TcoAnalysis(..), TcoExpr(..), TcoPop, TcoRef(..), TcoRole, TcoScope, TcoScopeItem)
import PureScript.Backend.Codegen.Tco as Tco
import PureScript.Backend.Convert (BackendBindingGroup, BackendModule, BackendImplementations)
import PureScript.Backend.Semantics (CtorMeta, DataTypeMeta, ExternImpl(..), NeutralExpr(..))
import PureScript.Backend.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))
import PureScript.CoreFn (ConstructorType(..), Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..), propValue, qualifiedModuleName, unQualified)

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
  { arguments :: NonEmptyArray (Tuple (Maybe Ident) Level)
  , body :: TcoExpr
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
  TcoExpr _ (Abs arguments body) | isTcoJoin tcoScope role ->
    Just { arguments, body }
  _ ->
    Nothing

boundTopLevel :: Ident -> CodegenEnv -> CodegenEnv
boundTopLevel ident env = env { bound = Map.insert ident 1 env.bound }

freshName :: CodegenRefType -> Maybe Ident -> Level -> CodegenEnv -> Tuple Ident CodegenEnv
freshName refType ident lvl env = case ident of
  Nothing ->
    Tuple (Ident ("_" <> show (unwrap lvl))) env
  Just id ->
    case Map.lookup id env.bound of
      Nothing ->
        Tuple id $ env
          { bound = Map.insert id 1 env.bound
          , names = Map.insert (CodegenLocal id lvl) (Tuple id refType) env.names
          }
      Just n -> do
        let fresh = Ident (unwrap id <> "_" <> show n)
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

noPure :: CodegenEnv -> CodegenEnv
noPure = _ { emitPure = false }

renameTopLevel :: Ident -> CodegenEnv -> CodegenName
renameTopLevel ident env = fromMaybe (Tuple ident RefStrict) $ Map.lookup (CodegenTopLevel ident) env.names

esCodegenModule :: forall a. CodegenOptions -> BackendImplementations -> BackendModule -> Dodo.Doc a
esCodegenModule options implementations mod@{ name: ModuleName this } = do
  let
    codegenEnv :: CodegenEnv
    codegenEnv =
      { currentModule: mod.name
      , bound: Map.empty
      , names: Map.empty
      , emitPure: true
      , options
      , implementations
      }

    foreignModuleName =
      ModuleName (this <> "$foreign")

    foreignBindings = map
      (\ident -> Tuple ident $ NeutralExpr $ Var $ Qualified (Just foreignModuleName) ident)
      mod.foreign

    moduleBindings =
      Array.cons { recursive: false, bindings: foreignBindings } mod.bindings >>= case _ of
        group@{ recursive: true } ->
          [ group ]
        { recursive: false, bindings } ->
          { recursive: false, bindings: _ } <<< pure <$> bindings

    exported = Set.fromFoldable $ map
      ( \(Tuple _ qual) -> case qual of
          Qualified Nothing ident ->
            Qualified (Just mod.name) ident
          _ -> qual
      )
      mod.exports

    docBindings =
      esCodegenTopLevelBindingGroup codegenEnv <$> moduleBindings

    usedBindings = foldr
      ( \{ group, used, lines } acc ->
          if any (flip Set.member exported) group || any (flip Set.member acc.used) group then
            { used: used <> acc.used, lines: lines <> acc.lines }
          else
            acc
      )
      { used: Set.empty, lines: [] }
      docBindings

    dataTypes =
      Map.toUnfoldable mod.dataTypes

    allExports = fold
      [ map ((\ty -> Tuple (esCtorIdent ty) (Qualified Nothing (esCtorIdent ty))) <<< fst) dataTypes
      , Array.mapMaybe
          ( case _ of
              Qualified (Just mn) ident | mn == mod.name ->
                Just $ Tuple ident $ Qualified Nothing ident
              _ ->
                Nothing
          )
          (Set.toUnfoldable (usedBindings.used <> exported))
      ]

    exportsByPath = allExports
      # Array.groupAllBy (comparing (qualifiedModuleName <<< snd))
      # Array.mapMaybe
          ( \as -> do
              let qual = qualifiedModuleName (snd (NonEmptyArray.head as))
              guard (isNothing qual || qual == Just foreignModuleName)
              pure $ Tuple (esModulePath <$> qual) (map unQualified <$> as)
          )

    statements = fold
      [ [ Statement esImportRuntime ]
      , (\mn -> Statement (esImport mn (esModulePath mn))) <$> mod.imports
      , Monoid.guard (not (Array.null foreignBindings)) [ Statement (esImport foreignModuleName (esForeignModulePath mod.name)) ]
      , map (Statement <<< uncurry (esCtorForType options)) dataTypes
      , map Statement usedBindings.lines
      , map (Statement <<< uncurry (maybe (esExports Nothing) (esExports <<< Just))) exportsByPath
      , Monoid.guard (not (Array.null foreignBindings)) [ Statement (esExportAllFrom (esForeignModulePath mod.name)) ]
      ]

  Monoid.guard (not (Array.null mod.comments))
    ( Dodo.lines (esComment <$> mod.comments)
        <> Dodo.break
    )
    <> esBlockStatements statements
    <> Dodo.break

needsPureWrapper :: TcoExpr -> Boolean
needsPureWrapper (TcoExpr _ expr) = case expr of
  Var _ -> false
  Local _ _ -> false
  Lit (LitRecord props) -> Array.any (needsPureWrapper <<< propValue) props
  Lit (LitArray values) -> Array.any needsPureWrapper values
  Lit _ -> false
  App a bs -> needsPureWrapper a || NonEmptyArray.any needsPureWrapper bs
  UncurriedApp a bs -> needsPureWrapper a || Array.any needsPureWrapper bs
  UncurriedEffectApp a bs -> needsPureWrapper a || Array.any needsPureWrapper bs
  Abs _ _ -> false
  UncurriedAbs _ _ -> false
  UncurriedEffectAbs _ _ -> false
  Accessor _ _ -> true
  Update a props -> needsPureWrapper a || Array.any (needsPureWrapper <<< propValue) props
  CtorSaturated _ _ _ _ props -> Array.any (needsPureWrapper <<< snd) props
  CtorDef _ _ _ _ -> false
  LetRec _ _ _ -> false
  Let _ _ _ _ -> false
  EffectBind _ _ _ _ -> false
  EffectPure _ -> false
  Branch _ _ -> false
  PrimOp _ -> true
  PrimEffect _ -> false
  PrimUndefined -> false
  Fail _ -> false

esCodegenTopLevelExpr :: forall a. CodegenEnv -> TcoExpr -> Dodo.Doc a
esCodegenTopLevelExpr env tcoExpr@(TcoExpr _ expr) = do
  let emitBlock = needsPureWrapper tcoExpr
  let exprDoc = esCodegenExpr (env { emitPure = not emitBlock }) tcoExpr
  if emitBlock then
    case expr of
      Lit (LitRecord _) ->
        esPure $ esBlock [ ReturnObject exprDoc ]
      _ ->
        esPure $ esBlock [ Return exprDoc ]
  else
    exprDoc

esCodegenExpr :: forall a. CodegenEnv -> TcoExpr -> Dodo.Doc a
esCodegenExpr env tcoExpr@(TcoExpr _ expr) = case expr of
  Var (Qualified (Just mn) ident) | mn == env.currentModule ->
    esCodegenName (renameTopLevel ident env)
  Var var ->
    esCodegenQualified var
  Local ident lvl ->
    esCodegenName (renameLocal ident lvl env)
  Lit lit ->
    esCodegenLit env lit
  App a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just doc <- shouldInlineApp env qual (NonEmptyArray.toArray bs) ->
            esPureEnv env $ doc
      _ ->
        esPureEnv env $ foldl
          ( \hd -> case _ of
              TcoExpr _ PrimUndefined ->
                esApp hd []
              arg ->
                esApp hd [ esCodegenExpr env arg ]
          )
          (esCodegenExpr env a)
          bs
  Abs idents body -> do
    let result = freshNames RefStrict env idents
    esCurriedFn result.value (esCodegenBlockStatements pureMode (noPure result.accum) body)
  UncurriedAbs idents body -> do
    let result = freshNames RefStrict env idents
    esFn result.value (esCodegenBlockStatements pureMode (noPure result.accum) body)
  UncurriedApp a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just doc <- shouldInlineApp env qual bs ->
            esPureEnv env $ doc
      _ ->
        esPureEnv env $ esApp (esCodegenExpr env a) (esCodegenExpr env <$> bs)
  UncurriedEffectAbs idents body -> do
    let result = freshNames RefStrict env idents
    esFn result.value (esCodegenBlockStatements effectMode (noPure result.accum) body)
  UncurriedEffectApp _ _ ->
    esPureEnv env $ esCodegenEffectBlock env tcoExpr
  Accessor a prop ->
    esCodegenAccessor (esCodegenExpr env a) prop
  Update a props ->
    esUpdate (esCodegenExpr env a) (map (esCodegenExpr env) <$> props)
  CtorDef ct ty tag [] -> do
    let ctorMeta = lookupCtorMeta env (Qualified (Just env.currentModule) tag)
    esPureEnv env $ esCtor env.options ct (Qualified Nothing (esCtorIdent ty)) tag ctorMeta []
  CtorDef ct ty tag fields -> do
    let ctorMeta = lookupCtorMeta env (Qualified (Just env.currentModule) tag)
    esCurriedFn (Ident <$> fields) [ Return (esCtor env.options ct (Qualified Nothing (esCtorIdent ty)) tag ctorMeta (esIdent <<< Ident <$> fields)) ]
  CtorSaturated (Qualified qual@(Just mn) _) ct ty tag fields | mn == env.currentModule -> do
    let ctorMeta = lookupCtorMeta env (Qualified qual tag)
    esPureEnv env $ esCtor env.options ct (Qualified Nothing (esCtorIdent ty)) tag ctorMeta (esCodegenExpr env <<< snd <$> fields)
  CtorSaturated (Qualified qual _) ct ty tag fields -> do
    let ctorMeta = lookupCtorMeta env (Qualified qual tag)
    esPureEnv env $ esCtor env.options ct (Qualified qual (esCtorIdent ty)) tag ctorMeta (esCodegenExpr env <<< snd <$> fields)
  PrimOp op ->
    esCodegenPrimOp env op
  PrimEffect _ ->
    esCodegenEffectBlock (noPure env) tcoExpr
  PrimUndefined ->
    esUndefined
  Fail "Failed pattern match" ->
    esFail
  Fail str ->
    esError str
  Branch _ _ ->
    esPureEnv env $ esCodegenBlock (noPure env) tcoExpr
  LetRec _ _ _ ->
    esPureEnv env $ esCodegenBlock (noPure env) tcoExpr
  Let _ _ _ _ ->
    esPureEnv env $ esCodegenBlock (noPure env) tcoExpr
  EffectBind _ _ _ _ ->
    esCodegenEffectBlock (noPure env) tcoExpr
  EffectPure _ ->
    esCodegenEffectBlock (noPure env) tcoExpr

shouldInlineApp :: forall a. CodegenEnv -> Qualified Ident -> Array TcoExpr -> Maybe (Dodo.Doc a)
shouldInlineApp env qual args = do
  fn <- Map.lookup qual esInlineMap
  fn (esCodegenExpr env) qual args

esCodegenBindEffect :: forall a. CodegenEnv -> TcoExpr -> Dodo.Doc a
esCodegenBindEffect env expr@(TcoExpr _ expr') = case expr' of
  UncurriedEffectApp a bs ->
    esApp (esCodegenExpr env a) (esCodegenExpr env <$> bs)
  PrimEffect a ->
    esCodegenPrimEffect env a
  _ ->
    esApp (esCodegenExpr env expr) []

esCodegenPrimEffect :: forall a. CodegenEnv -> BackendEffect TcoExpr -> Dodo.Doc a
esCodegenPrimEffect env = case _ of
  EffectRefNew a ->
    esRecord [ Prop "value" (esCodegenExpr env a) ]
  EffectRefRead a ->
    esAccessor (esCodegenExpr env a) "value"
  EffectRefWrite a b ->
    esAssignRef (esCodegenExpr env a) (esCodegenExpr env b)

esCodegenLit :: forall a. CodegenEnv -> Literal TcoExpr -> Dodo.Doc a
esCodegenLit env = case _ of
  LitInt n ->
    esInt n
  LitNumber n ->
    esNumber n
  LitString str ->
    esString str
  LitChar ch ->
    esChar ch
  LitBoolean bool ->
    Dodo.text (show bool)
  LitArray as ->
    esArray (esCodegenExpr env <$> as)
  LitRecord props ->
    esRecord (map (esCodegenExpr env) <$> props)

esCodegenBlock :: forall a. CodegenEnv -> TcoExpr -> Dodo.Doc a
esCodegenBlock env a = esBlock (esCodegenBlockStatements pureMode env a)

esCodegenEffectBlock :: forall a. CodegenEnv -> TcoExpr -> Dodo.Doc a
esCodegenEffectBlock env a = esEffectBlock (esCodegenBlockStatements effectMode env a)

type BlockMode =
  { effect :: Boolean
  , tco :: Boolean
  , tcoScope :: TcoScope
  , tcoJoins :: Set LocalRef
  }

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

esCodegenBlockStatements :: forall a. BlockMode -> CodegenEnv -> TcoExpr -> Array (EsStatement (Dodo.Doc a))
esCodegenBlockStatements = go []
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
              _ -> freshName RefStrict (Just (esTcoMutualIdent (_.name <$> tco))) lvl env'
          if isTcoJoin mode.tcoScope analysis.role then do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) mode
            let line = Statement $ esCodegenTcoMutualLoopBinding mode' env'' tcoIdent (NonEmptyArray.zip tcoNames tco)
            go (Array.snoc acc line) (foldr pushTcoJoin mode locals) env'' body
          else do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) (noTco mode)
            let line = Statement $ esCodegenTcoMutualLoopBinding mode' env'' tcoIdent (NonEmptyArray.zip tcoNames tco)
            go (Array.snoc acc line) mode env'' body
      | otherwise -> do
          let group = NonEmptyArray.toArray $ flip CodegenLocal lvl <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding env.currentModule group) bindings
          let result1 = freshBindingGroup RefLazy lvl env lazyBindings.no
          let result2 = freshBindingGroup RefStrict lvl result1.accum lazyBindings.yes
          let
            lines = fold
              [ Statement <<< uncurry (esCodegenBinding result2.accum) <$> result2.value
              , Statement <<< uncurry (esCodegenLazyBinding result2.accum) <$> result1.value
              , Statement <<< esCodegenLazyInit <<< fst <$> result1.value
              ]
          go (acc <> lines) mode (foldr strictCodegenRef result2.accum group) body
    Let ident lvl binding body
      | Just tco <- toTcoJoin mode.tcoScope analysis.role binding -> do
          let Tuple tcoIdent env' = freshName RefStrict ident lvl env
          let line = Statement $ esCodegenTcoJoinBinding mode env tcoIdent tco
          go (Array.snoc acc line) (pushTcoJoin (Tuple ident lvl) mode) env' body
      | otherwise -> do
          let Tuple newIdent env' = freshName RefStrict ident lvl env
          let line = Statement $ esCodegenBinding env newIdent binding
          go (Array.snoc acc line) mode env' body
    Branch bs def ->
      Array.snoc acc (Control (esCodegenBlockBranches mode env bs def))
    Fail _ ->
      Array.snoc acc (Statement (esCodegenExpr env tcoExpr))
    UncurriedEffectApp a bs | mode.effect -> do
      let line = esApp (esCodegenExpr env a) (esCodegenExpr env <$> bs)
      Array.snoc acc (Return line)
    EffectBind ident lvl eff body@(TcoExpr (TcoAnalysis { usages }) _) | mode.effect -> do
      let binding = esCodegenBindEffect env eff
      case Map.lookup (TcoLocal ident lvl) usages of
        Just (Usage { total }) | total > 0 -> do
          let Tuple newIdent env' = freshName RefStrict ident lvl env
          let line = Statement $ esBinding newIdent binding
          go (Array.snoc acc line) mode env' body
        _ -> do
          go (Array.snoc acc (Statement binding)) mode env body
    EffectPure expr' | mode.effect ->
      acc <> esCodegenBlockReturn (mode { effect = false }) env expr'
    App (TcoExpr _ (Local ident lvl)) bs
      | Just tco <- Tco.popTcoScope (TcoLocal ident lvl) mode.tcoScope ->
          acc <> esCodegenTcoJump mode tco (esCodegenExpr env <$> bs)
      | Set.member (Tuple ident lvl) mode.tcoJoins ->
          acc <> esCodegenTcoJoin mode (esCodegenExpr env tcoExpr)
    App (TcoExpr _ (Var qual)) bs | Just tco <- Tco.popTcoScope (TcoTopLevel qual) mode.tcoScope ->
      acc <> esCodegenTcoJump mode tco (esCodegenExpr env <$> bs)
    _ ->
      acc <> esCodegenBlockReturn mode env tcoExpr

esCodegenBlockReturn :: forall a. BlockMode -> CodegenEnv -> TcoExpr -> Array (EsStatement (Dodo.Doc a))
esCodegenBlockReturn mode env tcoExpr@(TcoExpr _ expr)
  | Just tco <- Tco.unwindTcoScope mode.tcoScope =
      esCodegenTcoReturn mode tco (esCodegenExpr env tcoExpr)
  | otherwise = case expr of
      PrimEffect eff | mode.effect -> do
        let doc = esCodegenPrimEffect env eff
        case eff of
          EffectRefNew _ ->
            [ ReturnObject doc ]
          _ ->
            [ Return doc ]
      _ -> do
        let doc = esCodegenExpr env tcoExpr
        case expr of
          Lit (LitRecord _) ->
            [ ReturnObject doc ]
          Update _ _ ->
            [ ReturnObject doc ]
          _ | mode.effect ->
            [ Return (esApp doc []) ]
          _ ->
            [ Return doc ]

esCodegenBlockBranches :: forall a. BlockMode -> CodegenEnv -> NonEmptyArray (Pair TcoExpr) -> Maybe TcoExpr -> Dodo.Doc a
esCodegenBlockBranches mode env bs def = esBranches (go <$> bs) (esCodegenBlockStatements mode env <$> def)
  where
  go :: Pair TcoExpr -> Tuple (Dodo.Doc a) (Array (EsStatement (Dodo.Doc a)))
  go (Pair a b@(TcoExpr _ b')) = case b' of
    Branch next nextDef ->
      Tuple (esCodegenExpr env a) [ Control (esCodegenBlockBranches mode env next nextDef) ]
    _ ->
      Tuple (esCodegenExpr env a) (esCodegenBlockStatements mode env b)

type TopLevelBindingGroup a =
  { used :: Set (Qualified Ident)
  , group :: Set (Qualified Ident)
  , lines :: Array (Dodo.Doc a)
  }

esCodegenTopLevelBindingGroup :: forall a. CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> TopLevelBindingGroup a
esCodegenTopLevelBindingGroup env { recursive, bindings }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings = do
      let tcoGroup = Tco.topLevelTcoEnvGroup env.currentModule bindings'
      let bindings'' = map (Tco.analyze tcoGroup) <$> bindings'
      let tcoRefBindings = Tco.topLevelTcoRefBindings env.currentModule bindings''
      let isLoop = maybe false Tco.tcoRoleIsLoop tcoRefBindings
      let used = foldMap (Tco.usedTopLevel <<< Tco.tcoAnalysisOf <<< snd) bindings''
      let groupNames = Set.fromFoldable $ map (Qualified (Just env.currentModule) <<< fst) bindings'
      case toTcoBindings { isLoop, joins: [] } bindings'' of
        Just tco -> do
          let tcoNames = _.name <$> tco
          let tcoIdent = esTcoMutualIdent tcoNames
          let tcoRefs = Tuple tcoIdent $ TcoTopLevel <<< Qualified (Just env.currentModule) <$> tcoNames
          let mode = pushTcoScope tcoRefs pureMode
          let env' = foldr (boundTopLevel) env tcoNames
          { used: Set.difference used groupNames
          , group: groupNames
          , lines: pure $ esCodegenTcoMutualLoopBinding mode (boundTopLevel tcoIdent env') tcoIdent (NonEmptyArray.zip tcoNames tco)
          }
        Nothing -> do
          let group = CodegenTopLevel <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding env.currentModule group) bindings''
          let env' = foldr (lazyTopLevel <<< fst) env lazyBindings.no
          { used: Set.difference used groupNames
          , group: groupNames
          , lines: fold
              [ uncurry (esCodegenTopLevelBinding env') <$> lazyBindings.yes
              , uncurry (esCodegenTopLevelLazyBinding env') <$> lazyBindings.no
              , esCodegenTopLevelLazyInit <<< fst <$> lazyBindings.no
              ]
          }
  | otherwise = do
      let bindings' = map (Tco.analyze []) <$> bindings
      { used: foldMap (Tco.usedTopLevel <<< Tco.tcoAnalysisOf <<< snd) bindings'
      , group: Set.fromFoldable $ map (Qualified (Just env.currentModule) <<< fst) bindings'
      , lines: map (\(Tuple ident b) -> esCodegenTopLevelBinding env ident b) bindings'
      }

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

esCodegenTopLevelBinding :: forall a. CodegenEnv -> Ident -> TcoExpr -> Dodo.Doc a
esCodegenTopLevelBinding env ident expr = esBinding ident (esCodegenTopLevelExpr env expr)

esCodegenBinding :: forall a. CodegenEnv -> Ident -> TcoExpr -> Dodo.Doc a
esCodegenBinding env ident expr = esBinding ident (esCodegenExpr env expr)

esCodegenTopLevelLazyBinding :: forall a. CodegenEnv -> Ident -> TcoExpr -> Dodo.Doc a
esCodegenTopLevelLazyBinding env ident expr = esTopLevelLazyBinding (esLazyIdent ident) (esCodegenBlockStatements pureMode (noPure env) expr)

esCodegenLazyBinding :: forall a. CodegenEnv -> Ident -> TcoExpr -> Dodo.Doc a
esCodegenLazyBinding env ident expr = esLazyBinding (esLazyIdent ident) (esCodegenBlockStatements pureMode env expr)

esCodegenTopLevelLazyInit :: forall a. Ident -> Dodo.Doc a
esCodegenTopLevelLazyInit ident = esBinding ident (esPure (esApp (esIdent (esLazyIdent ident)) []))

esCodegenLazyInit :: forall a. Ident -> Dodo.Doc a
esCodegenLazyInit ident = esBinding ident (esApp (esIdent (esLazyIdent ident)) [])

esCodegenTcoMutualLoopBinding :: forall a. BlockMode -> CodegenEnv -> Ident -> NonEmptyArray (Tuple Ident TcoBinding) -> Dodo.Doc a
esCodegenTcoMutualLoopBinding mode env tcoIdent bindings = case NonEmptyArray.toArray bindings of
  [ Tuple ident tco ] ->
    esCodegenTcoLoopBinding mode (noPure env) ident tco
  bindings' -> do
    let maxArgs = fromMaybe 0 $ maximum $ NonEmptyArray.length <<< _.arguments <<< snd <$> bindings
    let argIdents = esTcoArgIdent tcoIdent <$> Array.range 0 (maxArgs - 0)
    let branchIdent = esTcoBranchIdent tcoIdent
    esSepStatements $
      [ esBinding tcoIdent $ esTcoFn tcoIdent (NonEmptyArray.cons' branchIdent argIdents) $ esBranches
          ( mapWithIndex
              ( \ix (Tuple _ tco) -> do
                  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
                  Tuple (Dodo.words [ esIdent branchIdent, Dodo.text "===", esInt ix ]) $ fold
                    [ (\(Tuple arg var) -> Statement $ esBinding var (esIdent arg)) <$> Array.zip argIdents (NonEmptyArray.toArray argNames)
                    , esCodegenBlockStatements (mode { tco = true }) (noPure env') tco.body
                    ]
              )
              bindings
          )
          Nothing
      ]
        <>
          mapWithIndex
            ( \ix (Tuple ident _) ->
                esBinding ident $ esPure $ esApp (esIdent tcoIdent) [ esInt ix ]
            )
            bindings'

esCodegenTcoLoopBinding :: forall a. BlockMode -> CodegenEnv -> Ident -> TcoBinding -> Dodo.Doc a
esCodegenTcoLoopBinding mode env tcoIdent tco = do
  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
  let argIdents = mapWithIndex (Tuple <<< esTcoArgIdent tcoIdent) argNames
  esBinding tcoIdent $ esTcoFn tcoIdent (fst <$> argIdents) $ Dodo.lines
    [ esBlockStatements $ (\(Tuple arg var) -> Statement $ esBinding var (esIdent arg)) <$> argIdents
    , esBlockStatements $ esCodegenBlockStatements (mode { tco = true }) env' tco.body
    ]

esCodegenTcoJoinBinding :: forall a. BlockMode -> CodegenEnv -> Ident -> TcoJoin -> Dodo.Doc a
esCodegenTcoJoinBinding mode env tcoIdent tco = do
  let result = freshNames RefStrict env tco.arguments
  esBinding tcoIdent $ esCurriedFn result.value (esCodegenBlockStatements (mode { tco = false }) result.accum tco.body)

esCodegenTcoJump :: forall a. BlockMode -> TcoPop -> NonEmptyArray (Dodo.Doc a) -> Array (EsStatement (Dodo.Doc a))
esCodegenTcoJump mode pop args =
  stkStatements <>
    [ Statement $ esTcoApp pop args
    , Statement $ if mode.tco then esContinue else esReturn mempty
    ]
  where
  stkStatements =
    pop.stack
      # List.toUnfoldable
      # map (Statement <<< flip esAssign (Dodo.text "false") <<< esTcoLoopIdent)

esCodegenTcoReturn :: forall a. BlockMode -> Tuple Ident (List Ident) -> Dodo.Doc a -> Array (EsStatement (Dodo.Doc a))
esCodegenTcoReturn mode (Tuple tcoIdent stk) doc =
  stkStatements <>
    [ Statement $ esAssign (esTcoReturnIdent tcoIdent) doc
    , Statement $ if mode.tco then esContinue else esReturn mempty
    ]
  where
  stkStatements =
    List.Cons tcoIdent stk
      # List.toUnfoldable
      # map (Statement <<< flip esAssign (Dodo.text "false") <<< esTcoLoopIdent)

esCodegenTcoJoin :: forall a. BlockMode -> Dodo.Doc a -> Array (EsStatement (Dodo.Doc a))
esCodegenTcoJoin mode doc =
  [ Statement doc
  , Statement $ if mode.tco then esContinue else esReturn mempty
  ]

data EsOperatorTree b a
  = Binary (b -> b -> b) Int Int a a
  | Unary (b -> b) Int Int a
  | Leaf a

esCodegenPrimOp :: forall a. CodegenEnv -> BackendOperator TcoExpr -> Dodo.Doc a
esCodegenPrimOp = (\env -> go (Left 0) env <<< fromOperator env)
  where
  go :: Either Int Int -> CodegenEnv -> EsOperatorTree (Dodo.Doc a) TcoExpr -> Dodo.Doc a
  go prec env = case _ of
    Binary op p1 p2 lhs rhs -> do
      let
        doc = op
          (go (Left p1) env (fromExpr env lhs))
          (go (Right p1) env (fromExpr env rhs))
      case prec of
        Left n | p2 >= n ->
          doc
        Right n | p2 > n ->
          doc
        _ ->
          Dodo.Common.jsParens doc
    Unary op p1 p2 val -> do
      let doc = op (go (Right p1) env (fromExpr env val))
      case prec of
        Left n | p2 > n ->
          doc
        Right n | p2 >= n ->
          doc
        _ ->
          Dodo.Common.jsParens doc
    Leaf expr ->
      esCodegenExpr env expr

  fromExpr :: CodegenEnv -> TcoExpr -> EsOperatorTree (Dodo.Doc a) TcoExpr
  fromExpr env = case _ of
    TcoExpr _ (PrimOp expr) ->
      fromOperator env expr
    expr ->
      Leaf expr

  fromOperator :: CodegenEnv -> BackendOperator TcoExpr -> EsOperatorTree (Dodo.Doc a) TcoExpr
  fromOperator env = case _ of
    Op1 op1 a ->
      case op1 of
        OpBooleanNot -> opNot a
        OpIntBitNot -> opBitNeg a
        OpIntNegate -> opNeg a
        OpNumberNegate -> opNeg a
        OpArrayLength -> opArrayLength a
        OpIsTag op -> opIsTag env op a
    Op2 op2 a b ->
      case op2 of
        OpBooleanAnd -> opAnd a b
        OpBooleanOr -> opOr a b
        OpBooleanOrd op -> opOrd op a b
        OpCharOrd op -> opOrd op a b
        OpIntBitAnd -> opBitAnd a b
        OpIntBitOr -> opBitOr a b
        OpIntBitShiftLeft -> opShl a b
        OpIntBitShiftRight -> opShr a b
        OpIntBitXor -> opBitXor a b
        OpIntBitZeroFillShiftRight -> opZshr a b
        OpIntNum op -> opIntNum op a b
        OpIntOrd op -> opOrd op a b
        OpNumberNum op -> opNum op a b
        OpNumberOrd op -> opOrd op a b
        OpStringAppend -> opAdd a b
        OpStringOrd op -> opOrd op a b
        OpArrayIndex -> opArrayIndex a b

  binary op n =
    Binary (\a b -> Dodo.words [ a, Dodo.text op, b ]) n n

  binaryInt op n =
    Binary (\a b -> Dodo.words [ a, Dodo.text op, b, Dodo.text "|", Dodo.text "0" ]) n 3

  unary op n =
    Unary (\a -> Dodo.text op <> a) n n

  opOr = binary "||" 1
  opAnd = binary "&&" 2
  opBitOr = binary "|" 3
  opBitXor = binary "^" 4
  opBitAnd = binary "&" 5
  opEq = binary "===" 6
  opNotEq = binary "!==" 6
  opGt = binary ">" 7
  opGte = binary ">=" 7
  opLt = binary "<" 7
  opLte = binary "<=" 7
  opShl = binary "<<" 8
  opShr = binary ">>" 8
  opZshr = binary ">>>" 8
  opAdd = binary "+" 9
  opSub = binary "-" 9
  opIntAdd = binaryInt "+" 9
  opIntSub = binaryInt "-" 9
  opDiv = binary "/" 10
  opMul = binary "*" 10
  opIntDiv = binaryInt "/" 10
  opIntMul = binaryInt "*" 10
  opNeg = unary "-" 11
  opBitNeg = unary "~" 11
  opNot = unary "!" 11

  opNum = case _ of
    OpAdd -> opAdd
    OpDivide -> opDiv
    OpMultiply -> opMul
    OpSubtract -> opSub

  opOrd = case _ of
    OpEq -> opEq
    OpNotEq -> opNotEq
    OpGt -> opGt
    OpGte -> opGte
    OpLt -> opLt
    OpLte -> opLte

  opIntNum = case _ of
    OpAdd -> opIntAdd
    OpDivide -> opIntDiv
    OpMultiply -> opIntMul
    OpSubtract -> opIntSub

  opArrayLength =
    Unary (\a -> a <> Dodo.text ".length") top top

  opArrayIndex =
    Binary (\a b -> a <> Dodo.text "[" <> b <> Dodo.text "]") top top

  opIsTag env qual@(Qualified _ tag) = Unary
    ( \a -> Dodo.words
        [ a <> Dodo.text ".tag"
        , Dodo.text "==="
        , esTag env.options tag (lookupCtorMeta env qual)
        ]
    )
    top
    6

esCodegenAccessor :: forall a. Dodo.Doc a -> BackendAccessor -> Dodo.Doc a
esCodegenAccessor lhs = case _ of
  GetProp p ->
    esAccessor lhs p
  GetIndex n ->
    esIndex lhs n
  GetOffset n ->
    esOffset lhs n

esLocalIdent :: Maybe Ident -> Level -> Ident
esLocalIdent mb (Level lvl) = case mb of
  Just (Ident a) ->
    Ident (a <> "_" <> show lvl)
  Nothing ->
    Ident ("_" <> show lvl)

esCodegenName :: forall a. CodegenName -> Dodo.Doc a
esCodegenName (Tuple id refType) = case refType of
  RefStrict ->
    esIdent id
  RefLazy ->
    esApp (esIdent (esLazyIdent id)) []

esCodegenQualified :: forall a. Qualified Ident -> Dodo.Doc a
esCodegenQualified (Qualified qual ident) = case qual of
  Nothing -> esIdent ident
  Just mn -> esModuleName mn <> Dodo.text "." <> Dodo.text (esEscapeSpecial (unwrap ident))

esCtorIdent :: ProperName -> Ident
esCtorIdent (ProperName name) = Ident ("$" <> name)

esCtorForType :: forall a r. { intTags :: Boolean | r } -> ProperName -> DataTypeMeta -> Dodo.Doc a
esCtorForType options name meta = do
  let
    fields
      | meta.size > 0 = Ident <<< append "_" <<< show <$> Array.range 1 meta.size
      | otherwise = []
  case Map.toUnfoldable meta.constructors of
    [ Tuple ctor ctorMeta ] ->
      esBinding (esCtorIdent name) $ esFn fields
        [ ReturnObject $ Dodo.Common.jsCurlies
            $ Dodo.foldWithSeparator Dodo.Common.trailingComma
            -- Only add the tag for product types if we care what the name is,
            -- otherwise they are all 0 and it might as well not be there.
            $ (if options.intTags then identity else Array.cons (esProp (Prop "tag" (esTag options ctor ctorMeta))))
            $ esIdent <$> fields
        ]
    _ -> do
      let args = Array.cons (Ident "tag") fields
      esBinding (esCtorIdent name) $ esFn args
        [ ReturnObject $ Dodo.Common.jsCurlies
            $ Dodo.foldWithSeparator Dodo.Common.trailingComma
            $ esIdent <$> args
        ]

esTag :: forall a r. { intTags :: Boolean | r } -> Ident -> CtorMeta -> Dodo.Doc a
esTag options (Ident ctor) { tag }
  | options.intTags =
      Dodo.words [ esInt tag, Dodo.text "/*", Dodo.text ctor, Dodo.text "*/" ]
  | otherwise =
      esString ctor

esCtor :: forall a r. { intTags :: Boolean | r } -> ConstructorType -> Qualified Ident -> Ident -> CtorMeta -> Array (Dodo.Doc a) -> Dodo.Doc a
esCtor options ct fn ctor ctorMeta vals = case ct of
  SumType ->
    esApp (esCodegenQualified fn) $ Array.cons (esTag options ctor ctorMeta) vals
  ProductType ->
    esApp (esCodegenQualified fn) vals

esModulePath :: ModuleName -> String
esModulePath (ModuleName mn) = "../" <> mn <> "/index.js"

esForeignModulePath :: ModuleName -> String
esForeignModulePath (ModuleName _) = "./foreign.js"

esTcoMutualIdent :: NonEmptyArray Ident -> Ident
esTcoMutualIdent idents = case NonEmptyArray.toArray idents of
  [ ident ] -> ident
  _ -> Ident $ foldMap (String.take 5 <<< unwrap) idents

esTcoLoopIdent :: Ident -> Ident
esTcoLoopIdent (Ident tcoIdent) = Ident (tcoIdent <> "$c")

esTcoReturnIdent :: Ident -> Ident
esTcoReturnIdent (Ident tcoIdent) = Ident (tcoIdent <> "$r")

esTcoBranchIdent :: Ident -> Ident
esTcoBranchIdent (Ident tcoIdent) = Ident (tcoIdent <> "$b")

esTcoArgIdent :: Ident -> Int -> Ident
esTcoArgIdent (Ident tcoIdent) ix = Ident (tcoIdent <> "$" <> show ix)

esTcoCopyIdent :: Ident -> Ident
esTcoCopyIdent (Ident tcoIdent) = Ident (tcoIdent <> "$copy")

esTcoFn :: forall a. Ident -> NonEmptyArray Ident -> Dodo.Doc a -> Dodo.Doc a
esTcoFn tcoIdent args body = esCurriedFn (esTcoCopyIdent <$> args) $ fold
  [ (\arg -> Statement $ esLetBinding arg $ esIdent $ esTcoCopyIdent arg) <$> NonEmptyArray.toArray args
  , [ Statement $ esLetBinding (esTcoLoopIdent tcoIdent) (Dodo.text "true")
    , Statement $ esFwdRef (esTcoReturnIdent tcoIdent)
    , Statement $ Dodo.words
        [ Dodo.text "while"
        , Dodo.Common.jsParens (esIdent (esTcoLoopIdent tcoIdent))
        , Dodo.Common.jsCurlies body
        ]
    , Return (esIdent (esTcoReturnIdent tcoIdent))
    ]
  ]

esTcoApp :: forall a. TcoPop -> NonEmptyArray (Dodo.Doc a) -> Dodo.Doc a
esTcoApp pop args = esSepStatements $ fold
  [ Monoid.guard (NonEmptyArray.length pop.group > 1)
      [ esAssign (esTcoBranchIdent pop.ident) (esInt pop.index) ]
  , mapWithIndex (\ix arg -> esAssign (esTcoArgIdent pop.ident ix) arg) $ NonEmptyArray.toArray args
  ]

esTopLevelLazyBinding :: forall a. Ident -> Array (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esTopLevelLazyBinding ident bs = esBinding ident $ esPure $ esApp (Dodo.text "$runtime.binding") [ esFn [] bs ]

esLazyBinding :: forall a. Ident -> Array (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esLazyBinding ident bs = esBinding ident $ esApp (Dodo.text "$runtime.binding") [ esFn [] bs ]

esLazyIdent :: Ident -> Ident
esLazyIdent (Ident id) = Ident (id <> "$lazy")

esPureEnv :: forall a. CodegenEnv -> Dodo.Doc a -> Dodo.Doc a
esPureEnv { emitPure } doc
  | emitPure = esPure doc
  | otherwise = doc

esImportRuntime :: forall a. Dodo.Doc a
esImportRuntime = Dodo.words
  [ Dodo.text "import"
  , Dodo.text "*"
  , Dodo.text "as"
  , Dodo.text "$runtime"
  , Dodo.text "from"
  , esString "../runtime.js"
  ]

esFail :: forall a. Dodo.Doc a
esFail = Dodo.text "$runtime.fail()"

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
