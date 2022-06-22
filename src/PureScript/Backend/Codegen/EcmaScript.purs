module PureScript.Backend.Codegen.EcmaScript where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr, maximum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (maximumBy)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, Accum, mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Codegen.Tco (LocalRef, TcoAnalysis(..), TcoExpr(..), TcoRef(..), TcoRole, TcoScope, TcoScopeItem, TcoPop)
import PureScript.Backend.Codegen.Tco as Tco
import PureScript.Backend.Convert (BackendBindingGroup, BackendModule)
import PureScript.Backend.Semantics (NeutralExpr(..))
import PureScript.Backend.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))
import PureScript.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..), qualifiedModuleName, unQualified)

data CodegenName = CodegenIdent Ident

type CodegenEnv =
  { currentModule :: ModuleName
  , bound :: Map Ident Int
  , names :: Map (Tuple Ident Level) Ident
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

toTcoBindings :: TcoRole -> Array (Tuple Ident TcoExpr) -> Maybe (Array TcoBinding)
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

freshName :: Maybe Ident -> Level -> CodegenEnv -> Tuple Ident CodegenEnv
freshName ident lvl env = case ident of
  Nothing ->
    Tuple (Ident ("_" <> show (unwrap lvl))) env
  Just id ->
    case Map.lookup id env.bound of
      Nothing ->
        Tuple id $ env
          { bound = Map.insert id 1 env.bound
          , names = Map.insert (Tuple id lvl) id env.names
          }
      Just n -> do
        let fresh = Ident (unwrap id <> "_" <> show n)
        Tuple fresh $ env
          { bound = Map.insert id (n + 1) env.bound
          , names = Map.insert (Tuple id lvl) fresh env.names
          }

freshNames :: forall f. Traversable f => CodegenEnv -> f LocalRef -> Accum CodegenEnv (f Ident)
freshNames = mapAccumL \env' (Tuple ident level) -> do
  let Tuple newIdent env'' = freshName ident level env'
  { accum: env'', value: newIdent }

freshBindingGroup :: forall f a. Traversable f => Level -> CodegenEnv -> f (Tuple Ident a) -> Accum CodegenEnv (f (Tuple Ident a))
freshBindingGroup level = mapAccumL \env' (Tuple ident binding) -> do
  let Tuple newIdent env'' = freshName (Just ident) level env'
  { accum: env''
  , value: Tuple newIdent binding
  }

rename :: Maybe Ident -> Level -> CodegenEnv -> Ident
rename ident lvl env =
  case ident >>= \id -> Map.lookup (Tuple id lvl) env.names of
    Nothing ->
      esLocalIdent ident lvl
    Just id ->
      id

esCodegenModule :: forall a. BackendModule -> Dodo.Doc a
esCodegenModule mod@{ name: ModuleName this } = do
  let
    foreignModuleName =
      ModuleName (this <> "$foreign")

    foreignBindings = map
      (\ident -> Tuple ident $ NeutralExpr $ Var $ Qualified (Just foreignModuleName) ident)
      mod.foreign

    moduleBindings =
      Array.cons { recursive: false, bindings: foreignBindings } mod.bindings

    ctorSizes =
      (moduleBindings >>= _.bindings)
        # Array.mapMaybe case _ of
            Tuple _ (NeutralExpr (CtorDef ty _ fields)) ->
              Just (Tuple ty (Array.length fields))
            _ ->
              Nothing
        # Array.groupAllBy (comparing fst)
        # map (maximumBy (comparing snd))

    codegenEnv :: CodegenEnv
    codegenEnv =
      { currentModule: mod.name
      , bound: Map.empty
      , names: Map.empty
      }

    exportsByPath = mod.exports
      # append ((\ty -> Tuple (esCtorIdent ty) (Qualified Nothing (esCtorIdent ty))) <<< fst <$> ctorSizes)
      # Array.groupAllBy (comparing (qualifiedModuleName <<< snd))
      # map (\as -> Tuple (esModulePath <$> qualifiedModuleName (snd (NonEmptyArray.head as))) (map unQualified <$> as))

  esBlockStatements $ fold
    [ (\mn -> Statement (esImport mn (esModulePath mn))) <$> mod.imports
    , Monoid.guard (not (Array.null foreignBindings)) [ Statement (esImport foreignModuleName (esForeignModulePath mod.name)) ]
    , map (Statement <<< uncurry esCtorForType) ctorSizes
    , map Statement $ esCodegenTopLevelBindingGroup codegenEnv =<< moduleBindings
    , map (Statement <<< uncurry (maybe (esExports Nothing) (esExports <<< Just))) exportsByPath
    , Monoid.guard (not (Array.null foreignBindings)) [ Statement (esExportAllFrom (esForeignModulePath mod.name)) ]
    ]

esCodegenExpr :: forall a. CodegenEnv -> TcoExpr -> Dodo.Doc a
esCodegenExpr env tcoExpr@(TcoExpr _ expr) = case expr of
  Var (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) ->
    esUndefined
  Var (Qualified (Just mn) ident) | mn == env.currentModule ->
    esCodegenIdent ident
  Var var ->
    esCodegenQualified var
  Local ident lvl ->
    esCodegenIdent (rename ident lvl env)
  Lit lit ->
    esCodegenLit env lit
  App a bs
    | [ TcoExpr _ (Var (Qualified (Just (ModuleName "Prim")) (Ident "undefined"))) ] <- NonEmptyArray.toArray bs ->
        esApp (esCodegenExpr env a) []
    | otherwise ->
        esCurriedApp (esCodegenExpr env a) (esCodegenExpr env <$> bs)
  Abs idents body
    | [ Tuple (Just (Ident "$__unused")) _ ] <- NonEmptyArray.toArray idents ->
        esFn [] (esCodegenBlockStatements pureMode env body)
    | otherwise -> do
        let result = freshNames env idents
        esCurriedFn result.value (esCodegenBlockStatements pureMode result.accum body)
  UncurriedAbs idents body -> do
    let result = freshNames env idents
    esFn result.value (esCodegenBlockStatements pureMode result.accum body)
  UncurriedApp a bs ->
    esApp (esCodegenExpr env a) (esCodegenExpr env <$> bs)
  UncurriedEffectAbs idents body -> do
    let result = freshNames env idents
    esFn result.value (esCodegenBlockStatements effectMode result.accum body)
  UncurriedEffectApp _ _ ->
    esCodegenEffectBlock env tcoExpr
  Accessor a prop ->
    esCodegenAccessor (esCodegenExpr env a) prop
  Update a props ->
    esUpdate (esCodegenExpr env a) (map (esCodegenExpr env) <$> props)
  CtorDef ty (Ident tag) [] ->
    esCtor (Qualified Nothing (esCtorIdent ty)) tag []
  CtorDef ty (Ident tag) fields ->
    esCurriedFn fields [ Return (esCtor (Qualified Nothing (esCtorIdent ty)) tag (esCodegenIdent <$> fields)) ]
  CtorSaturated (Qualified (Just mn) _) ty (Ident tag) fields | mn == env.currentModule ->
    esCtor (Qualified Nothing (esCtorIdent ty)) tag (esCodegenExpr env <<< snd <$> fields)
  CtorSaturated (Qualified qual _) ty (Ident tag) fields ->
    esCtor (Qualified qual (esCtorIdent ty)) tag (esCodegenExpr env <<< snd <$> fields)
  PrimOp op ->
    esCodegenPrimOp env op
  Fail str ->
    esError str
  Branch _ _ ->
    esCodegenBlock env tcoExpr
  LetRec _ _ _ ->
    esCodegenBlock env tcoExpr
  Let _ _ _ _ ->
    esCodegenBlock env tcoExpr
  EffectBind _ _ _ _ ->
    esCodegenEffectBlock env tcoExpr
  EffectPure _ ->
    esCodegenEffectBlock env tcoExpr

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

data EsStatement a
  = Statement a
  | Control a
  | Return a
  | ReturnObject a

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
          let { value: tcoNames, accum: env' } = freshNames env locals
          let
            Tuple tcoIdent env'' = case tcoNames of
              [ tcoIdent ] -> Tuple tcoIdent env'
              _ -> freshName (Just (esTcoMutualIdent (_.name <$> tco))) lvl env'
          if isTcoJoin mode.tcoScope analysis.role then do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) mode
            let line = Statement $ esCodegenTcoMutualLoopBinding mode' env'' tcoIdent (Array.zip tcoNames tco)
            go (Array.snoc acc line) (foldr pushTcoJoin mode locals) env'' body
          else do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) (noTco mode)
            let line = Statement $ esCodegenTcoMutualLoopBinding mode' env'' tcoIdent (Array.zip tcoNames tco)
            go (Array.snoc acc line) mode env'' body
      | otherwise -> do
          let result = freshBindingGroup lvl env bindings
          let lines = Statement <$> esCodegenBindingGroup result.accum { recursive: true, bindings: result.value }
          go (acc <> lines) mode result.accum body
    Let ident lvl binding body
      | Just tco <- toTcoJoin mode.tcoScope analysis.role binding -> do
          let Tuple tcoIdent env' = freshName ident lvl env
          let line = Statement $ esCodegenTcoJoinBinding mode env tcoIdent tco
          go (Array.snoc acc line) (pushTcoJoin (Tuple ident lvl) mode) env' body
      | otherwise -> do
          let Tuple newIdent env' = freshName ident lvl env
          let line = Statement $ esCodegenBinding env newIdent binding
          go (Array.snoc acc line) mode env' body
    Branch bs def ->
      Array.snoc acc (Control (esCodegenBlockBranches mode env bs def))
    Fail _ ->
      Array.snoc acc (Statement (esCodegenExpr env tcoExpr))
    UncurriedEffectApp a bs | mode.effect -> do
      let line = esApp (esCodegenExpr env a) (esCodegenExpr env <$> bs)
      Array.snoc acc (Return line)
    EffectBind ident lvl (TcoExpr _ (UncurriedEffectApp a bs)) body | mode.effect -> do
      let Tuple newIdent env' = freshName ident lvl env
      let line = Statement $ esBinding newIdent (esApp (esCodegenExpr env a) (esCodegenExpr env <$> bs))
      go (Array.snoc acc line) mode env' body
    EffectBind (Just (Ident "$__unused")) _ eff body | mode.effect -> do
      let line = Statement $ esApp (esCodegenExpr env eff) []
      go (Array.snoc acc line) mode env body
    EffectBind ident lvl eff body | mode.effect -> do
      let Tuple newIdent env' = freshName ident lvl env
      let
        -- TODO: handle left associated effect binddings
        line = Statement $ esBinding newIdent $ case eff of
          TcoExpr _ (EffectBind _ _ _ _) ->
            esApp (Dodo.text "(" <> esCodegenExpr env eff <> Dodo.text ")") []
          _ ->
            esApp (esCodegenExpr env eff) []
      go (Array.snoc acc line) mode env' body
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
  | otherwise = do
      let doc = esCodegenExpr env tcoExpr
      case expr of
        Lit (LitRecord _) ->
          [ ReturnObject doc ]
        _ | mode.effect ->
          [ Return (esApp doc []) ]
        _ ->
          [ Return doc ]

esCodegenBlockBranches :: forall a. BlockMode -> CodegenEnv -> Array (Pair TcoExpr) -> Maybe TcoExpr -> Dodo.Doc a
esCodegenBlockBranches mode env bs def = esBranches (go <$> bs) (esCodegenBlockStatements mode env <$> def)
  where
  go :: Pair TcoExpr -> Tuple (Dodo.Doc a) (Array (EsStatement (Dodo.Doc a)))
  go (Pair a b@(TcoExpr _ b')) = case b' of
    Branch next nextDef ->
      Tuple (esCodegenExpr env a) [ Control (esCodegenBlockBranches mode env next nextDef) ]
    _ ->
      Tuple (esCodegenExpr env a) (esCodegenBlockStatements mode env b)

esCodegenTopLevelBindingGroup :: forall a. CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> Array (Dodo.Doc a)
esCodegenTopLevelBindingGroup env { recursive, bindings }
  | recursive = do
      let group = Tco.topLevelTcoEnvGroup env.currentModule bindings
      let bindings' = map (Tco.analyze group) <$> bindings
      let tcoRefBindings = Tco.topLevelTcoRefBindings env.currentModule bindings'
      let isLoop = maybe false Tco.tcoRoleIsLoop tcoRefBindings
      case toTcoBindings { isLoop, joins: [] } bindings' of
        Just tco -> do
          let tcoNames = _.name <$> tco
          let tcoIdent = esTcoMutualIdent tcoNames
          let tcoRefs = Tuple tcoIdent $ TcoTopLevel <<< Qualified (Just env.currentModule) <$> tcoNames
          let mode = pushTcoScope tcoRefs pureMode
          pure $ esCodegenTcoMutualLoopBinding mode (boundTopLevel tcoIdent env) tcoIdent (Array.zip tcoNames tco)
        Nothing -> do
          let names = fst <$> bindings
          let fwdRefs = esFwdRef <$> names
          fwdRefs <> map (\(Tuple ident b) -> esAssign ident (esCodegenExpr env b)) bindings'
  | otherwise =
      map (\(Tuple ident b) -> esBinding ident (esCodegenExpr env (Tco.analyze [] b))) bindings

esCodegenBindingGroup :: forall a. CodegenEnv -> BackendBindingGroup Ident TcoExpr -> Array (Dodo.Doc a)
esCodegenBindingGroup env { recursive, bindings }
  | recursive = do
      let fwdRefs = esFwdRef <<< fst <$> bindings
      fwdRefs <> map (\(Tuple ident b) -> esAssign ident (esCodegenExpr env b)) bindings
  | otherwise =
      uncurry (esCodegenBinding env) <$> bindings

esCodegenBinding :: forall a. CodegenEnv -> Ident -> TcoExpr -> Dodo.Doc a
esCodegenBinding env ident expr = esBinding ident (esCodegenExpr env expr)

esCodegenTcoMutualLoopBinding :: forall a. BlockMode -> CodegenEnv -> Ident -> Array (Tuple Ident TcoBinding) -> Dodo.Doc a
esCodegenTcoMutualLoopBinding mode env tcoIdent = case _ of
  [ Tuple ident tco ] -> esCodegenTcoLoopBinding mode env ident tco
  bindings -> do
    let maxArgs = fromMaybe 0 $ maximum $ NonEmptyArray.length <<< _.arguments <<< snd <$> bindings
    let argIdents = esTcoArgIdent tcoIdent <$> Array.range 0 (maxArgs - 0)
    let branchIdent = esTcoBranchIdent tcoIdent
    esSepStatements $
      [ esBinding tcoIdent $ esTcoFn tcoIdent (NonEmptyArray.cons' branchIdent argIdents) $ esBranches
          ( mapWithIndex
              ( \ix (Tuple _ tco) -> do
                  let { value: argNames, accum: env' } = freshNames env tco.arguments
                  Tuple (Dodo.words [ esCodegenIdent branchIdent, Dodo.text "===", esInt ix ]) $ fold
                    [ (\(Tuple arg var) -> Statement $ esBinding var (esCodegenIdent arg)) <$> Array.zip argIdents (NonEmptyArray.toArray argNames)
                    , esCodegenBlockStatements (mode { tco = true }) env' tco.body
                    ]
              )
              bindings
          )
          Nothing
      ]
        <>
          mapWithIndex
            ( \ix (Tuple ident _) ->
                esBinding ident $ esApp (esCodegenIdent tcoIdent) [ esInt ix ]
            )
            bindings

esCodegenTcoLoopBinding :: forall a. BlockMode -> CodegenEnv -> Ident -> TcoBinding -> Dodo.Doc a
esCodegenTcoLoopBinding mode env tcoIdent tco = do
  let { value: argNames, accum: env' } = freshNames env tco.arguments
  let argIdents = mapWithIndex (Tuple <<< esTcoArgIdent tcoIdent) argNames
  esBinding tcoIdent $ esTcoFn tcoIdent (fst <$> argIdents) $ Dodo.lines
    [ esBlockStatements $ (\(Tuple arg var) -> Statement $ esBinding var (esCodegenIdent arg)) <$> argIdents
    , esBlockStatements $ esCodegenBlockStatements (mode { tco = true }) env' tco.body
    ]

esCodegenTcoJoinBinding :: forall a. BlockMode -> CodegenEnv -> Ident -> TcoJoin -> Dodo.Doc a
esCodegenTcoJoinBinding mode env tcoIdent tco = do
  let result = freshNames env tco.arguments
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
esCodegenPrimOp = (\env -> go (Left 0) env <<< fromOperator)
  where
  go :: Either Int Int -> CodegenEnv -> EsOperatorTree (Dodo.Doc a) TcoExpr -> Dodo.Doc a
  go prec env = case _ of
    Binary op p1 p2 lhs rhs -> do
      let
        doc = op
          (go (Left p1) env (fromExpr lhs))
          (go (Right p1) env (fromExpr rhs))
      case prec of
        Left n | p2 >= n ->
          doc
        Right n | p2 > n ->
          doc
        _ ->
          Dodo.Common.jsParens doc
    Unary op p1 p2 val -> do
      let doc = op (go (Right p1) env (fromExpr val))
      case prec of
        Left n | p2 > n ->
          doc
        Right n | p2 >= n ->
          doc
        _ ->
          Dodo.Common.jsParens doc
    Leaf expr ->
      esCodegenExpr env expr

  fromExpr :: TcoExpr -> EsOperatorTree (Dodo.Doc a) TcoExpr
  fromExpr = case _ of
    TcoExpr _ (PrimOp expr) ->
      fromOperator expr
    expr ->
      Leaf expr

  fromOperator :: BackendOperator TcoExpr -> EsOperatorTree (Dodo.Doc a) TcoExpr
  fromOperator = case _ of
    Op1 op1 a ->
      case op1 of
        OpBooleanNot -> opNot a
        OpIntBitNot -> opBitNeg a
        OpIntNegate -> opNeg a
        OpNumberNegate -> opNeg a
        OpArrayLength -> opArrayLength a
        OpIsTag op -> opIsTag op a
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

  opIsTag (Qualified _ (Ident tag)) = Unary
    ( \a -> Dodo.words
        [ a <> Dodo.text ".tag"
        , Dodo.text "==="
        , esString tag
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

esCodegenIdent :: forall a. Ident -> Dodo.Doc a
esCodegenIdent (Ident a) = Dodo.text (esEscapeIdent a)

esCodegenQualified :: forall a. Qualified Ident -> Dodo.Doc a
esCodegenQualified (Qualified qual ident) = case qual of
  Nothing -> esCodegenIdent ident
  Just mn -> esCodegenModuleName mn <> Dodo.text "." <> Dodo.text (esEscapeSpecial (unwrap ident))

esCodegenModuleName :: forall a. ModuleName -> Dodo.Doc a
esCodegenModuleName (ModuleName mn) = Dodo.text (esEscapeIdent mn)

esEscapeIdent :: String -> String
esEscapeIdent = escapeReserved
  where
  escapeReserved str
    | Set.member str esReservedNames =
        "$$" <> str
    | otherwise =
        esEscapeSpecial str

esEscapeSpecial :: String -> String
esEscapeSpecial =
  String.replaceAll (String.Pattern "'") (String.Replacement "$p")
    >>> String.replaceAll (String.Pattern ".") (String.Replacement "$d")

esReservedNames :: Set String
esReservedNames = Set.fromFoldable
  [ "AggregateError"
  , "Array"
  , "ArrayBuffer"
  , "AsyncFunction"
  , "AsyncGenerator"
  , "AsyncGeneratorFunction"
  , "Atomics"
  , "BigInt"
  , "BigInt64Array"
  , "BigUint64Array"
  , "Boolean"
  , "Boolean"
  , "DataView"
  , "Date"
  , "Error"
  , "EvalError"
  , "Float32Array"
  , "Float64Array"
  , "Function"
  , "Generator"
  , "GeneratorFunction"
  , "Infinity"
  , "Int16Array"
  , "Int32Array"
  , "Int8Array"
  , "Intl"
  , "JSON"
  , "Map"
  , "Math"
  , "NaN"
  , "Number"
  , "Object"
  , "Promise"
  , "Proxy"
  , "RangeError"
  , "ReferenceError"
  , "Reflect"
  , "RegExp"
  , "Set"
  , "SharedArrayBuffer"
  , "String"
  , "Symbol"
  , "SyntaxError"
  , "TypeError"
  , "URIError"
  , "Uint16Array"
  , "Uint32Array"
  , "Uint8Array"
  , "Uint8ClampedArray"
  , "WeakMap"
  , "WeakSet"
  , "WebAssembly"
  , "abstract"
  , "arguments"
  , "await"
  , "boolean"
  , "break"
  , "byte"
  , "case"
  , "catch"
  , "char"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "eval"
  , "export"
  , "extends"
  , "false"
  , "final"
  , "finally"
  , "float"
  , "for"
  , "function"
  , "get"
  , "globalThis"
  , "goto"
  , "if"
  , "implements"
  , "import"
  , "in"
  , "instanceof"
  , "int"
  , "interface"
  , "let"
  , "long"
  , "native"
  , "new"
  , "null"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "return"
  , "set"
  , "short"
  , "static"
  , "super"
  , "switch"
  , "synchronized"
  , "this"
  , "throw"
  , "throws"
  , "transient"
  , "true"
  , "try"
  , "typeof"
  , "undefined"
  , "var"
  , "void"
  , "volatile"
  , "while"
  , "with"
  , "yield"
  ]

esFwdRef :: forall a. Ident -> Dodo.Doc a
esFwdRef ident = Dodo.text "let" <> Dodo.space <> esCodegenIdent ident

esLetBinding :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
esLetBinding ident b = Dodo.words
  [ Dodo.text "let"
  , esCodegenIdent ident
  , Dodo.text "="
  , b
  ]

esBinding :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
esBinding ident b = Dodo.words
  [ Dodo.text "const"
  , esCodegenIdent ident
  , Dodo.text "="
  , b
  ]

esAssign :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
esAssign ident b = Dodo.words
  [ esCodegenIdent ident
  , Dodo.text "="
  , b
  ]

esAssignProp :: forall a. Ident -> Prop (Dodo.Doc a) -> Dodo.Doc a
esAssignProp ident (Prop prop val) = fold
  [ esCodegenIdent ident
  , Dodo.Common.jsSquares (Dodo.text (show prop))
  , Dodo.space
  , Dodo.text "="
  , Dodo.space
  , val
  ]

esAccessor :: forall a. Dodo.Doc a -> String -> Dodo.Doc a
esAccessor expr prop = case esEscapeProp prop of
  Nothing ->
    expr <> Dodo.text "." <> Dodo.text prop
  Just escaped ->
    expr <> Dodo.Common.jsSquares (Dodo.text escaped)

esIndex :: forall a. Dodo.Doc a -> Int -> Dodo.Doc a
esIndex expr ix = expr <> Dodo.text "[" <> Dodo.text (show ix) <> Dodo.text "]"

esOffset :: forall a. Dodo.Doc a -> Int -> Dodo.Doc a
esOffset expr ix = expr <> Dodo.text "._" <> Dodo.text (show (ix + 1))

esUpdate :: forall a. Dodo.Doc a -> Array (Prop (Dodo.Doc a)) -> Dodo.Doc a
esUpdate rec props = Dodo.Common.jsCurlies $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ Array.cons (Dodo.text "..." <> rec) (esProp <$> props)

esBlock :: forall a. Array (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esBlock stmts = Dodo.text "(" <> esFn mempty stmts <> Dodo.text ")" <> Dodo.text "()"

esEffectBlock :: forall a. Array (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esEffectBlock stmts = esFn mempty stmts

esBlockStatements :: forall f a. Foldable f => Functor f => f (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esBlockStatements = Dodo.lines <<< map go
  where
  go = case _ of
    Statement a -> a <> Dodo.text ";"
    Control a -> a
    Return a -> esReturn a <> Dodo.text ";"
    ReturnObject a -> esReturn a <> Dodo.text ";"

esFn :: forall a. Array Ident -> Array (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esFn args stmts = Dodo.words
  [ if Array.length args == 1 then
      foldMap esCodegenIdent args
    else
      Dodo.Common.jsParens (Dodo.foldWithSeparator Dodo.Common.trailingComma (esCodegenIdent <$> args))
  , Dodo.text "=>"
  , esFnBody stmts
  ]

esReturn :: forall a. Dodo.Doc a -> Dodo.Doc a
esReturn doc = Dodo.words
  [ Dodo.text "return"
  , doc
  ]

esCurriedFn :: forall f a. Foldable f => f Ident -> Array (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esCurriedFn args stmts = foldr go (esFnBody stmts) args
  where
  go arg body = Dodo.words
    [ esCodegenIdent arg
    , Dodo.text "=>"
    , body
    ]

esFnBody :: forall a. Array (EsStatement (Dodo.Doc a)) -> Dodo.Doc a
esFnBody = case _ of
  [] -> Dodo.Common.jsCurlies mempty
  [ Return a ] -> a
  [ ReturnObject a ] -> Dodo.Common.jsParens a
  stmts -> Dodo.Common.jsCurlies (esBlockStatements stmts)

esArray :: forall a. Array (Dodo.Doc a) -> Dodo.Doc a
esArray = Dodo.Common.jsSquares <<< Dodo.foldWithSeparator Dodo.Common.trailingComma

esRecord :: forall a. Array (Prop (Dodo.Doc a)) -> Dodo.Doc a
esRecord = Dodo.Common.jsCurlies <<< Dodo.foldWithSeparator Dodo.Common.trailingComma <<< map esProp

esProp :: forall a. Prop (Dodo.Doc a) -> Dodo.Doc a
esProp (Prop prop val) = fold
  [ Dodo.text (fromMaybe prop $ esEscapeProp prop)
  , Dodo.text ":"
  , Dodo.space
  , val
  ]

esEscapeProp :: String -> Maybe String
esEscapeProp = \prop ->
  if Regex.test safeRegex prop then
    Nothing
  else
    Just $ show prop
  where
  safeRegex = unsafeRegex """^[a-zA-Z_$][a-zA-Z0-9_$]*$""" noFlags

esCtorIdent :: ProperName -> Ident
esCtorIdent (ProperName name) = Ident ("$" <> name)

esCtorForType :: forall a. ProperName -> Int -> Dodo.Doc a
esCtorForType name len =
  esBinding (esCtorIdent name) $ esFn args
    [ ReturnObject $ Dodo.Common.jsCurlies
        $ Dodo.foldWithSeparator Dodo.Common.trailingComma
        $ esCodegenIdent <$> args
    ]
  where
  args =
    Array.cons (Ident "tag") fields

  fields
    | len > 0 = Ident <<< append "_" <<< show <$> Array.range 1 len
    | otherwise = []

esCtor :: forall a. Qualified Ident -> String -> Array (Dodo.Doc a) -> Dodo.Doc a
esCtor fn tag vals = esApp (esCodegenQualified fn) $ Array.cons (esString tag) vals

esString :: forall a. String -> Dodo.Doc a
esString = Dodo.text <<< show

esNumber :: forall a. Number -> Dodo.Doc a
esNumber = Dodo.text <<< show

esInt :: forall a. Int -> Dodo.Doc a
esInt = Dodo.text <<< show

esChar :: forall a. Char -> Dodo.Doc a
esChar = Dodo.text <<< show

esBoolean :: forall a. Boolean -> Dodo.Doc a
esBoolean = Dodo.text <<< show

esApp :: forall a. Dodo.Doc a -> Array (Dodo.Doc a) -> Dodo.Doc a
esApp a bs =
  if Array.length bs == 1 then
    a <> Dodo.text "(" <> Dodo.flexGroup args <> Dodo.text ")"
  else
    a <> Dodo.Common.jsParens args
  where
  args = Dodo.foldWithSeparator Dodo.Common.trailingComma bs

esCurriedApp :: forall a. Dodo.Doc a -> NonEmptyArray (Dodo.Doc a) -> Dodo.Doc a
esCurriedApp = foldl (\a b -> esApp a [ b ])

esIfElse :: forall f a. Foldable f => f (Tuple (Dodo.Doc a) (Dodo.Doc a)) -> Dodo.Doc a -> Dodo.Doc a
esIfElse conds default = Dodo.lines
  [ condChain.doc
  , Monoid.guard (not (Dodo.isEmpty default)) $ fold
      [ Dodo.text "else"
      , Dodo.space
      , Dodo.Common.jsCurlies default
      ]
  ]
  where
  condChain = foldl go { elseif: false, doc: mempty } conds
  go { elseif, doc } (Tuple cond body) =
    { elseif: true
    , doc: fold
        [ doc
        , if elseif then Dodo.space <> Dodo.text "else if" else Dodo.text "if"
        , Dodo.space
        , Dodo.Common.jsParens cond
        , Dodo.space
        , Dodo.Common.jsCurlies body
        ]
    }

esBranches :: forall a. Array (Tuple (Dodo.Doc a) (Array (EsStatement (Dodo.Doc a)))) -> Maybe (Array (EsStatement (Dodo.Doc a))) -> Dodo.Doc a
esBranches branches def =
  Dodo.lines
    [ Dodo.lines $ map
        ( \(Tuple doc stmts) -> Dodo.flexGroup $ fold
            [ Dodo.text "if"
            , Dodo.space
            , Dodo.Common.jsParens doc
            , Dodo.space
            , Dodo.text "{"
            , Dodo.spaceBreak
            , Dodo.indent (esBlockStatements stmts)
            , Dodo.spaceBreak
            , Dodo.text "}"
            ]
        )
        branches
    , foldMap esBlockStatements def
    ]

esImport :: forall a. ModuleName -> String -> Dodo.Doc a
esImport mn path = Dodo.words
  [ Dodo.text "import"
  , Dodo.text "*"
  , Dodo.text "as"
  , esCodegenModuleName mn
  , Dodo.text "from"
  , Dodo.text (show path)
  ]

esExports :: forall a. Maybe String -> NonEmptyArray (Tuple Ident Ident) -> Dodo.Doc a
esExports mbPath exports = Dodo.words
  [ Dodo.text "export"
  , Dodo.Common.jsCurlies $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ map
      ( \(Tuple id1 id2) -> do
          let id1' = esEscapeSpecial (unwrap id1)
          let id2' = esEscapeIdent (unwrap id2)
          if id1' == id2' || isJust mbPath then
            Dodo.text id1'
          else
            Dodo.words
              [ Dodo.text id2'
              , Dodo.text "as"
              , Dodo.text id1'
              ]
      )
      exports
  , flip foldMap mbPath \path -> Dodo.words
      [ Dodo.text "from"
      , Dodo.text (show path)
      ]
  ]

esExportAllFrom :: forall a. String -> Dodo.Doc a
esExportAllFrom path = Dodo.words
  [ Dodo.text "export"
  , Dodo.text "*"
  , Dodo.text "from"
  , Dodo.text (show path)
  ]

esModulePath :: ModuleName -> String
esModulePath (ModuleName mn) = "./" <> mn <> ".js"

esForeignModulePath :: ModuleName -> String
esForeignModulePath (ModuleName mn) = "./" <> mn <> ".foreign.js"

esUndefined :: forall a. Dodo.Doc a
esUndefined = Dodo.text "undefined"

esError :: forall a. String -> Dodo.Doc a
esError str = Dodo.words
  [ Dodo.text "throw"
  , Dodo.text "new"
  , esApp (Dodo.text "Error") [ esString str ]
  ]

esTcoMutualIdent :: Array Ident -> Ident
esTcoMutualIdent = case _ of
  [ ident ] -> ident
  idents -> Ident $ foldMap (String.take 5 <<< unwrap) idents

esTcoLoopIdent :: Ident -> Ident
esTcoLoopIdent (Ident tcoIdent) = Ident (tcoIdent <> "$c")

esTcoReturnIdent :: Ident -> Ident
esTcoReturnIdent (Ident tcoIdent) = Ident (tcoIdent <> "$r")

esTcoBranchIdent :: Ident -> Ident
esTcoBranchIdent (Ident tcoIdent) = Ident (tcoIdent <> "$b")

esTcoArgIdent :: Ident -> Int -> Ident
esTcoArgIdent (Ident tcoIdent) ix = Ident (tcoIdent <> "$" <> show ix)

esTcoFn :: forall a. Ident -> NonEmptyArray Ident -> Dodo.Doc a -> Dodo.Doc a
esTcoFn tcoIdent args body = esCurriedFn args
  [ Statement $ esLetBinding (esTcoLoopIdent tcoIdent) (Dodo.text "true")
  , Statement $ esFwdRef (esTcoReturnIdent tcoIdent)
  , Statement $ Dodo.words
      [ Dodo.text "while"
      , Dodo.Common.jsParens (esCodegenIdent (esTcoLoopIdent tcoIdent))
      , Dodo.Common.jsCurlies body
      ]
  , Return (esCodegenIdent (esTcoReturnIdent tcoIdent))
  ]

esTcoApp :: forall a. TcoPop -> NonEmptyArray (Dodo.Doc a) -> Dodo.Doc a
esTcoApp pop args = esSepStatements $ fold
  [ Monoid.guard (Array.length pop.group > 1)
      [ esAssign (esTcoBranchIdent pop.ident) (esInt pop.index) ]
  , mapWithIndex (\ix arg -> esAssign (esTcoArgIdent pop.ident ix) arg) $ NonEmptyArray.toArray args
  ]

esContinue :: forall a. Dodo.Doc a
esContinue = Dodo.text "continue"

esSepStatements :: forall f a. Foldable f => f (Dodo.Doc a) -> Dodo.Doc a
esSepStatements = Dodo.foldWithSeparator (Dodo.text ";" <> Dodo.break)
