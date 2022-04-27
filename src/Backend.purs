module Backend where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.RWS (RWST, ask, censor, evalRWST, listen, modify_, pass, state, tell)
import Control.Monad.Writer (class MonadWriter)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bisequence, bitraverse)
import Data.Foldable (class Foldable, fold, foldl, foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function (applyFlipped, on)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Dodo as Dodo
import Dodo.Common (jsCurlies, jsParens, jsSquares, trailingComma)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Meta(..), Module(..), ModuleName(..), Prop(..), Qualified(..), ReExport(..), propValue)

type BackendEnv =
  { coreFnModules :: Map ModuleName (Module Ann)
  , backendModules :: Map ModuleName BackendModule
  , currentModule :: ModuleName
  }

type ConstructorMeta =
  { name :: Ident
  , fields :: Array Ident
  }

type BackendState =
  { fresh :: Int
  , constructors :: Map (Qualified Ident) ConstructorMeta
  }

data BackendAnalysis
  = Summarized BackendAnalysisSummary
  | Capture BackendAnalysis
  | Intro Ident BackendAnalysis BackendAnalysis
  | IntroRec (Array (Tuple Ident BackendAnalysis)) BackendAnalysis
  | Append BackendAnalysis BackendAnalysis
  | Empty

instance Semigroup BackendAnalysis where
  append = case _, _ of
    Empty, b -> b
    a, Empty -> a
    Summarized as, Summarized bs -> Summarized (as <> bs)
    a, b -> Append a b

instance Monoid BackendAnalysis where
  mempty = Empty

newtype BackendAnalysisSummary = BackendAnalysisSummary
  { usages :: Map Ident Usage
  }

instance Semigroup BackendAnalysisSummary where
  append (BackendAnalysisSummary a) (BackendAnalysisSummary b) = BackendAnalysisSummary
    { usages: Map.unionWith append a.usages b.usages
    }

instance Monoid BackendAnalysisSummary where
  mempty = BackendAnalysisSummary { usages: Map.empty }

newtype Usage = Usage
  { count :: Int
  , captured :: Boolean
  , tailCall :: Boolean
  }

instance Semigroup Usage where
  append (Usage a) (Usage b) = Usage
    { count: a.count + b.count
    , captured: a.captured || b.captured
    , tailCall: a.tailCall && b.tailCall
    }

used :: { tailCall :: Boolean } -> Qualified Ident -> BackendAnalysis
used { tailCall } (Qualified mn ident)
  | isJust mn = Empty
  | otherwise = Summarized $ BackendAnalysisSummary
      { usages: Map.singleton ident $ Usage { count: 1, captured: false, tailCall }
      }

summarize :: BackendAnalysis -> BackendAnalysisSummary
summarize = case _ of
  Summarized s -> s
  Capture (Capture b) ->
    summarize (Capture b)
  Capture b -> do
    let BackendAnalysisSummary { usages } = summarize b
    BackendAnalysisSummary
      { usages: (\(Usage u) -> Usage (u { captured = true })) <$> usages
      }
  Intro id a b -> do
    let BackendAnalysisSummary { usages } = summarize b
    summarize a <> BackendAnalysisSummary
      { usages: Map.delete id usages
      }
  IntroRec as b -> do
    let BackendAnalysisSummary { usages } = summarize $ foldMap snd as <> b
    BackendAnalysisSummary
      { usages: foldr (Map.delete <<< fst) usages as
      }
  Append a b ->
    summarize a <> summarize b
  Empty ->
    mempty

passify :: forall w m a. MonadWriter w m => (w -> w) -> m a -> m (Tuple a w)
passify k = pass <<< map (flip Tuple k) <<< listen

passify_ :: forall w m a. MonadWriter w m => m a -> m (Tuple a w)
passify_ = passify (const mempty)

type BackendM = RWST BackendEnv BackendAnalysis BackendState Identity

runBackendM :: forall a. BackendEnv -> Int -> BackendM a -> a
runBackendM env fresh m = fst $ un Identity $ evalRWST m env { fresh, constructors: Map.empty }

tmpIdent :: BackendM Ident
tmpIdent = state \st -> Tuple (Ident ("$t" <> show st.fresh)) (st { fresh = st.fresh + 1 })

type BackendBindingGroup =
  { recursive :: Boolean
  , bindings :: Array (Tuple Ident BackendExpr)
  }

newtype BackendModule = BackendModule
  { imports :: Array (Tuple ModuleName String)
  , bindings :: Array BackendBindingGroup
  , exports :: Array (Tuple Ident (Qualified Ident))
  }

backendModule :: Module Ann -> BackendM BackendModule
backendModule (Module mod@{ name: ModuleName this }) = do
  bindings <- backendTopLevelBindingGroups mod.decls
  let foreignModuleName = ModuleName (this <> "$foreign")
  pure $ BackendModule
    { imports: fold
        [ Array.mapMaybe
            ( \(Import _ mn) ->
                guard (mn /= mod.name && mn /= ModuleName "Prim")
                  $> Tuple mn (luaModulePath mn)
            )
            mod.imports
        , Monoid.guard (not (Array.null mod.foreign))
            [ Tuple foreignModuleName (luaForeignModulePath mod.name) ]
        ]
    , bindings: fold
        [ [ { recursive: false
            , bindings: map (\ident -> Tuple ident (Var (Qualified (Just foreignModuleName) ident))) mod.foreign
            }
          ]
        , bindings
        ]
    , exports: fold
        [ map (\a -> Tuple a (Qualified Nothing a)) mod.exports
        , map (\(ReExport mn a) -> Tuple a (Qualified (Just mn) a)) mod.reExports
        ]
    }

data BackendExpr
  = Var (Qualified Ident)
  | Lit (Literal BackendExpr)
  | App BackendExpr (NonEmptyArray BackendExpr)
  | Abs (NonEmptyArray Ident) BackendExpr
  | Accessor BackendExpr BackendAccessor
  | Update BackendExpr (Array (Prop BackendExpr))
  | CtorDef Ident (Array Ident)
  | LetRec (Array (Tuple Ident BackendExpr)) BackendExpr
  | Let Ident BackendExpr BackendExpr
  | Branch (Array (Tuple BackendExpr BackendExpr))
  | Test BackendExpr BackendGuard
  | Fail String
  | OptInlineComp BackendExpr (NonEmptyArray BackendExpr)
  | OptInline (Map Ident BackendExpr) BackendExpr
  -- | LetJoin (Array Ident) (Array JoinPoint) BackendExpr
  -- | Join Ident (Array (Tuple Ident BackendExpr))

-- data JoinPoint = JoinPoint Ident Int BackendExpr

data BackendAccessor
  = GetProp String
  | GetIndex Int
  | GetOffset Int

data BackendGuard
  = GuardNumber Number
  | GuardInt Int
  | GuardString String
  | GuardBoolean Boolean
  | GuardChar Char
  | GuardTag (Qualified Ident)
  | GuardArrayLength Int

data PatternStk
  = PatBinder (Binder Ann) PatternStk
  | PatPush BackendAccessor PatternStk
  | PatPop PatternStk
  | PatNil

backendExpr :: Expr Ann -> BackendM BackendExpr
backendExpr = go
  where
  go :: Expr Ann -> BackendM BackendExpr
  go expr = rewrite (goExpr expr)

  rewrite :: BackendM BackendExpr -> BackendM BackendExpr
  rewrite expr = pass do
    Tuple expr' analysis' <- listen expr
    pure $ map const $ inlineExpr (optimizeExpr (simplifyExpr expr')) analysis'

  simplifyExpr :: BackendExpr -> BackendExpr
  simplifyExpr = case _ of
    App (App a bs) cs ->
      App a (bs <> cs)
    Abs as (Abs bs expr) ->
      Abs (as <> bs) expr
    other ->
      other

  optimizeExpr :: BackendExpr -> BackendExpr
  optimizeExpr = case _ of
    App (Var (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident "compose"))) args
      | [ (Var (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident "semigroupoidFn"))), f, g ] <- NonEmptyArray.toArray args ->
          case f, g of
            OptInlineComp hd1 tl1, OptInlineComp hd2 tl2 ->
              OptInlineComp hd1 (tl1 <> NonEmptyArray.cons hd2 tl2)
            OptInlineComp hd1 tl1, _ ->
              OptInlineComp hd1 (NonEmptyArray.snoc tl1 g)
            _, OptInlineComp hd1 tl1 ->
              OptInlineComp f (NonEmptyArray.cons hd1 tl1)
            _, _ ->
              OptInlineComp f (NonEmptyArray.singleton g)
    other ->
      other

  inlineExpr :: BackendExpr -> BackendAnalysis -> Tuple BackendExpr BackendAnalysis
  inlineExpr expr analysis =
    case expr, analysis of
      Let id b@(Var _) c, Intro _ b' c' -> do
        let s2@(BackendAnalysisSummary { usages }) = summarize c'
        case Map.lookup id usages of
          Just (Usage { count: 1, captured: false }) ->
            Tuple (OptInline (Map.singleton id b) c) (b' <> Summarized s2)
          _ ->
            Tuple expr (Intro id b' (Summarized s2))
      _, _ ->
        Tuple expr analysis

  -- isSimpleExpr :: BackendExpr -> Boolean
  -- isSimpleExpr = case _ of
  --   Var _ -> true
  --   -- Accessor a _ -> isSimpleExpr a
  --   -- App a bs -> isSimpleExpr a && NonEmptyArray.all isSimpleExpr bs
  --   -- OptInline _ a -> isSimpleExpr a
  --   -- Lit (LitArray as) -> Array.all isSimpleExpr as
  --   -- Lit (LitRecord as) -> Array.all (isSimpleExpr <<< propValue) as
  --   -- Lit _ -> true
  --   _ -> false

  goExpr :: Expr Ann -> BackendM BackendExpr
  goExpr = case _ of
    ExprVar _ qi@(Qualified mn ident) -> do
      { currentModule } <- ask
      let
        qi'
          | mn == Just currentModule = Qualified Nothing ident
          | otherwise = qi
      tell $ used { tailCall: false } qi'
      pure (Var qi')
    ExprLit _ lit ->
      Lit <$> traverse go lit
    ExprConstructor _ _ name fields ->
      pure (CtorDef name fields)
    ExprAccessor _ a field ->
      flip Accessor (GetProp field) <$> go a
    ExprUpdate _ a bs ->
      Update <$> go a <*> traverse (traverse go) bs
    ExprAbs _ arg body ->
      Abs (NonEmptyArray.singleton arg) <$> censor Capture (go body)
    ExprApp _ a b
      | ExprVar (Ann { meta: Just IsNewtype }) _ <- a ->
          go b
      | otherwise ->
          App <$> go a <*> (NonEmptyArray.singleton <$> go b)
    ExprCase _ exprs alts -> do
      scrutinees <- traverse (\e -> flip Tuple (goExpr e) <$> tmpIdent) exprs
      let branches = map (goAlt (fst <$> scrutinees)) alts
      foldr (uncurry makeLet) (foldr (lift2 mergeBranches) (makeBranch [ Tuple (makeLit (LitBoolean true)) (makeFail "Failed pattern match") ]) branches) scrutinees
      where
      mergeBranches = case _, _ of
        Branch bs1, Branch bs2
          | Just (Tuple (Lit (LitBoolean true)) _) <- Array.last bs1 ->
              Branch bs1
          | otherwise ->
              Branch (bs1 <> bs2)
        nonBranch, _ ->
          nonBranch
    ExprLet _ binds body ->
      backendBindingGroups binds (goExpr body)

  goAlt :: Array Ident -> CaseAlternative Ann -> BackendM BackendExpr
  goAlt idents (CaseAlternative binders branch) =
    goBinders
      ( \renames ->
          foldr (\(Tuple a b) -> makeLet a (makeVar (Qualified Nothing b))) (goCaseGuard branch) renames
      )
      List.Nil
      List.Nil
      (List.fromFoldable idents)
      (foldr (\b s -> PatBinder b (PatPop s)) PatNil binders)

  goCaseGuard :: CaseGuard Ann -> BackendM BackendExpr
  goCaseGuard = case _ of
    Unconditional expr ->
      go expr
    Guarded gs ->
      Branch <$> traverse (\(Guard a b) -> Tuple <$> go a <*> go b) gs

  goBinders
    :: (List (Tuple Ident Ident) -> BackendM BackendExpr)
    -> List (BackendM BackendExpr -> BackendM BackendExpr)
    -> List (Tuple Ident Ident)
    -> List Ident
    -> PatternStk
    -> BackendM BackendExpr
  goBinders k ks store stk = case _ of
    PatBinder binder next ->
      case binder, stk of
        BinderNull _, _ ->
          goBinders k ks store stk next
        BinderVar _ a, List.Cons id _ ->
          goBinders k ks (List.Cons (Tuple a id) store) stk next
        BinderNamed _ a b, List.Cons id _ ->
          goBinders k ks (List.Cons (Tuple a id) store) stk (PatBinder b next)
        BinderLit _ lit, List.Cons id _ ->
          case lit of
            LitInt n ->
              goBinders k (List.Cons (makeGuard id (GuardInt n)) ks) store stk next
            LitNumber n ->
              goBinders k (List.Cons (makeGuard id (GuardNumber n)) ks) store stk next
            LitString n ->
              goBinders k (List.Cons (makeGuard id (GuardString n)) ks) store stk next
            LitChar n ->
              goBinders k (List.Cons (makeGuard id (GuardChar n)) ks) store stk next
            LitBoolean n ->
              goBinders k (List.Cons (makeGuard id (GuardBoolean n)) ks) store stk next
            LitArray bs ->
              goBinders k (List.Cons (makeGuard id (GuardArrayLength (Array.length bs))) ks) store stk
                $ foldrWithIndex (\ix b s -> PatPush (GetIndex ix) $ PatBinder b $ PatPop s) next bs
            LitRecord ps ->
              goBinders k ks store stk
                $ foldr (\(Prop ix b) s -> PatPush (GetProp ix) $ PatBinder b $ PatPop s) next ps
        BinderConstructor (Ann { meta: Just IsNewtype }) _ _ [ b ], _ ->
          goBinders k ks store stk (PatBinder b next)
        BinderConstructor _ _ tag bs, List.Cons id _ ->
          goBinders k (List.Cons (makeGuard id (GuardTag tag)) ks) store stk
            $ foldrWithIndex (\ix b s -> PatPush (GetOffset ix) $ PatBinder b $ PatPop s) next bs
        _, _ ->
          unsafeCrashWith "impossible: goBinders (binder)"
    PatPush accessor next ->
      case stk of
        List.Cons id _ -> do
          tmp <- tmpIdent
          goBinders k (List.Cons (makeLet tmp (makeAccessor (makeVar (Qualified Nothing id)) accessor)) ks) store (List.Cons tmp stk) next
        _ ->
          unsafeCrashWith "impossible: goBinders (push)"
    PatPop next ->
      case stk of
        List.Cons _ stk' ->
          goBinders k ks store stk' next
        List.Nil ->
          unsafeCrashWith "impossible: goBinders (pop)"
    PatNil ->
      foldl applyFlipped (k store) ks

  makeGuard :: Ident -> BackendGuard -> BackendM BackendExpr -> BackendM BackendExpr
  makeGuard id g inner = rewrite do
    inner' <- inner
    let qi = Qualified Nothing id
    tell $ used { tailCall: false } qi
    pure $ Branch [ Tuple (Test (Var qi) g) inner' ]

  makeLet :: Ident -> BackendM BackendExpr -> BackendM BackendExpr -> BackendM BackendExpr
  makeLet id a b = rewrite do
    Tuple a' w1 <- passify_ a
    Tuple b' w2 <- passify_ b
    tell $ Intro id w1 w2
    pure $ Let id a' b'

  makeAccessor :: BackendM BackendExpr -> BackendAccessor -> BackendM BackendExpr
  makeAccessor v acc = rewrite $ flip Accessor acc <$> v

  makeVar :: Qualified Ident -> BackendM BackendExpr
  makeVar qi = rewrite do
    tell $ used { tailCall: false } qi
    pure (Var qi)

  makeBranch :: Array (Tuple (BackendM BackendExpr) (BackendM BackendExpr)) -> BackendM BackendExpr
  makeBranch = map Branch <<< traverse bisequence

  makeFail :: String -> BackendM BackendExpr
  makeFail = pure <<< Fail

  makeLit :: Literal (BackendM BackendExpr) -> BackendM BackendExpr
  makeLit = map Lit <<< sequence

  backendBindingGroups :: Array (Bind Ann) -> BackendM BackendExpr -> BackendM BackendExpr
  backendBindingGroups binds body = foldr goBind body binds
    where
    goBind :: Bind Ann -> BackendM BackendExpr -> BackendM BackendExpr
    goBind bind' next = rewrite case bind' of
      Rec bindings -> do
        bindings' <- traverse (passify_ <<< backendBinding) bindings
        Tuple next' w <- passify_ next
        tell $ IntroRec (lmap fst <$> bindings') w
        pure $ LetRec (fst <$> bindings') next'
      NonRec binding -> do
        Tuple (Tuple ident binding') w1 <- passify_ (backendBinding binding)
        Tuple next' w2 <- passify_ next
        tell $ Intro ident w1 w2
        pure $ Let ident binding' next'

backendTopLevelBindingGroups :: Array (Bind Ann) -> BackendM (Array BackendBindingGroup)
backendTopLevelBindingGroups binds = do
  binds' <- traverse backendTopLevelBindingGroup binds
  pure $ (\as -> { recursive: (NonEmptyArray.head as).recursive, bindings: _.bindings =<< NonEmptyArray.toArray as }) <$>
    Array.groupBy ((&&) `on` (not <<< _.recursive)) binds'

backendTopLevelBindingGroup :: Bind Ann -> BackendM BackendBindingGroup
backendTopLevelBindingGroup = case _ of
  Rec bindings ->
    { recursive: true, bindings: _ } <$> traverse backendBinding bindings
  NonRec binding ->
    { recursive: false, bindings: _ } <<< pure <$> backendBinding binding

backendBinding :: Binding Ann -> BackendM (Tuple Ident BackendExpr)
backendBinding (Binding _ ident expr) = do
  res <- backendExpr expr
  case res of
    CtorDef name fields -> do
      { currentModule } <- ask
      addConstructor (Qualified (Just currentModule) ident) { name, fields }
    _ -> pure unit
  pure (Tuple ident res)

addConstructor :: Qualified Ident -> ConstructorMeta -> BackendM Unit
addConstructor ident meta = modify_ \state ->
  state
    { constructors =
        Map.insert ident meta state.constructors
    }

codegenModule :: forall a. BackendModule -> BackendM (Dodo.Doc a)
codegenModule (BackendModule mod) = do
  stmts <- join <$> traverse codegenBindingGroup mod.bindings
  pure $ Dodo.lines
    [ Dodo.lines $ uncurry luaImport <$> mod.imports
    , Dodo.lines stmts
    , Dodo.words
        [ Dodo.text "return"
        , luaRecord (map (uncurry luaExport) mod.exports)
        ]
    ]

codegenExpr :: forall a. BackendExpr -> BackendM (Dodo.Doc a)
codegenExpr = case _ of
  Var (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) ->
    pure luaUndefined
  Var var ->
    pure $ codegenQualified codegenIdent var
  Lit lit ->
    codegenLit lit
  App a bs ->
    luaCurriedApp <$> codegenExpr a <*> traverse codegenExpr bs
  Abs idents body
    | [ Ident "$__unused" ] <- NonEmptyArray.toArray idents ->
        luaFn [] <$> codegenBlockStatements body
    | otherwise ->
        luaCurriedFn idents <$> codegenBlockStatements body
  Accessor a prop ->
    flip codegenAccessor prop <$> codegenExpr a
  Update a props -> do
    tmp1 <- tmpIdent
    tmp2 <- tmpIdent
    luaCopyTo tmp1 tmp2
      <$> codegenExpr a
      <*> map (map (luaAssignProp tmp2)) (traverse (traverse codegenExpr) props)
  CtorDef (Ident tag) fields ->
    pure $ luaCurriedFn fields (luaCtor tag (codegenIdent <$> fields))
  Test a b ->
    flip codegenTest b <$> codegenExpr a
  Fail str ->
    pure $ luaError str
  OptInlineComp hd tl -> do
    bindings <- for (NonEmptyArray.toArray (NonEmptyArray.cons hd tl)) \expr ->
      Tuple <$> tmpIdent <*> codegenExpr expr
    arg <- tmpIdent
    pure $ luaBlock $ luaStatements $ fold
      [ uncurry luaBinding <$> bindings
      , [ luaFn [ arg ] $ luaReturn
            ( foldr (\(Tuple fn _) a -> luaApp (codegenIdent fn) [ a ])
                (codegenIdent arg)
                bindings
            )
        ]
      ]
  OptInline rewrites next ->
    codegenExpr (evalInline rewrites next)
  expr@(Branch _) ->
    codegenBlock expr
  expr@(LetRec _ _) ->
    codegenBlock expr
  expr@(Let _ _ _) ->
    codegenBlock expr

evalInline :: Map Ident BackendExpr -> BackendExpr -> BackendExpr
evalInline rewrites = case _ of
  Var (Qualified Nothing ident) | Just expr <- Map.lookup ident rewrites ->
    expr
  expr@(Var _) ->
    expr
  Let ident a b ->
    Let ident (OptInline rewrites a) (OptInline (Map.delete ident rewrites) b)
  LetRec idents b -> do
    let rewrites' = foldr (Map.delete <<< fst) rewrites idents
    LetRec (map (OptInline rewrites') <$> idents) (OptInline rewrites' b)
  Lit (LitArray as) ->
    Lit (LitArray (OptInline rewrites <$> as))
  Lit (LitRecord as) ->
    Lit (LitRecord (map (OptInline rewrites) <$> as))
  expr@(Lit _) ->
    expr
  App a bs ->
    App (OptInline rewrites a) (OptInline rewrites <$> bs)
  Abs idents b ->
    Abs idents (OptInline (foldr Map.delete rewrites idents) b)
  Accessor a b ->
    Accessor (OptInline rewrites a) b
  Update a bs ->
    Update (OptInline rewrites a) (map (OptInline rewrites) <$> bs)
  Branch bs ->
    Branch (bimap (OptInline rewrites) (OptInline rewrites) <$> bs)
  Test a b ->
    Test (OptInline rewrites a) b
  OptInline rewrites' next ->
    evalInline (Map.union rewrites' rewrites) next
  OptInlineComp a bs ->
    OptInlineComp (OptInline rewrites a) (OptInline rewrites <$> bs)
  expr@(Fail _) ->
    expr
  expr@(CtorDef _ _) ->
    expr

codegenBlock :: forall a. BackendExpr -> BackendM (Dodo.Doc a)
codegenBlock expr = luaBlock <$> codegenBlockStatements expr

codegenBlockBranches :: forall a. Array (Tuple BackendExpr BackendExpr) -> BackendM (Dodo.Doc a)
codegenBlockBranches = (\bs -> luaBranches <$> traverse (bitraverse codegenExpr go) bs)
  where
  go = case _ of
    Branch bs ->
      codegenBlockBranches bs
    expr ->
      codegenBlockStatements expr

codegenBlockStatements :: forall a. BackendExpr -> BackendM (Dodo.Doc a)
codegenBlockStatements = map luaStatements <<< go []
  where
  go acc = case _ of
    LetRec bindings body -> do
      lines <- codegenBindingGroup { recursive: true, bindings }
      go (acc <> lines) body
    Let ident expr body -> do
      lines <- codegenBindingGroup { recursive: false, bindings: [ Tuple ident expr ] }
      go (acc <> lines) body
    Branch bs ->
      Array.snoc acc <$> codegenBlockBranches bs
    expr@(Fail _) ->
      Array.snoc acc <$> codegenExpr expr
    expr ->
      Array.snoc acc <<< luaReturn <$> codegenExpr expr

codegenTest :: forall a. Dodo.Doc a -> BackendGuard -> Dodo.Doc a
codegenTest lhs = case _ of
  GuardNumber n ->
    Dodo.words [ lhs, Dodo.text "==", luaNumber n ]
  GuardInt n ->
    Dodo.words [ lhs, Dodo.text "==", luaInt n ]
  GuardString str ->
    Dodo.words [ lhs, Dodo.text "==", luaString str ]
  GuardBoolean bool ->
    Dodo.words [ lhs, Dodo.text "==", luaBoolean bool ]
  GuardChar ch ->
    Dodo.words [ lhs, Dodo.text "==", luaChar ch ]
  GuardTag (Qualified _ (Ident tag)) ->
    Dodo.words [ luaIndex lhs 0, Dodo.text "==", luaString tag ]
  GuardArrayLength len ->
    Dodo.words [ lhs <> Dodo.text ".n", Dodo.text "==", luaInt len ]

codegenAccessor :: forall a. Dodo.Doc a -> BackendAccessor -> Dodo.Doc a
codegenAccessor lhs = case _ of
  GetProp p ->
    luaAccessor lhs p
  GetIndex n ->
    luaIndex lhs n
  GetOffset n ->
    luaOffset lhs n

codegenBindingGroup :: forall a. BackendBindingGroup -> BackendM (Array (Dodo.Doc a))
codegenBindingGroup { recursive, bindings }
  | recursive = do
      let fwdRefs = luaFwdRef <<< fst <$> bindings
      (fwdRefs <> _) <$> traverse (\(Tuple ident b) -> luaAssign ident <$> codegenExpr b) bindings
  | otherwise =
      traverse (\(Tuple ident b) -> luaBinding ident <$> codegenExpr b) bindings

codegenLit :: forall a. Literal BackendExpr -> BackendM (Dodo.Doc a)
codegenLit = case _ of
  LitInt n ->
    pure $ luaInt n
  LitNumber n ->
    pure $ luaNumber n
  LitString str ->
    pure $ luaString str
  LitChar ch ->
    pure $ luaChar ch
  LitBoolean bool ->
    pure $ Dodo.text (show bool)
  LitArray as ->
    luaArray <$> traverse codegenExpr as
  LitRecord props ->
    luaRecord <$> traverse (traverse codegenExpr) props

codegenIdent :: forall a. Ident -> Dodo.Doc a
codegenIdent (Ident a) = Dodo.text (escapeIdent a)

codegenQualified :: forall a b. (a -> Dodo.Doc b) -> Qualified a -> Dodo.Doc b
codegenQualified codegenInner (Qualified qual inner) = case qual of
  Nothing -> codegenInner inner
  Just mn -> codegenModuleName mn <> Dodo.text "." <> codegenInner inner

codegenModuleName :: forall a. ModuleName -> Dodo.Doc a
codegenModuleName (ModuleName mn) = Dodo.text (escapeIdent mn)

escapeIdent :: String -> String
escapeIdent = escapeReserved
  where
  escapeReserved str
    | Set.member str reservedNames =
        str <> "_r"
    | otherwise =
        escapeSpecial str

  escapeSpecial =
    String.replaceAll (Pattern "_") (Replacement "_u")
      >>> String.replaceAll (Pattern "'") (Replacement "_p")
      >>> String.replaceAll (Pattern ".") (Replacement "_")
      >>> String.replaceAll (Pattern "$") (Replacement "__")

  reservedNames = Set.fromFoldable
    [ "and"
    , "args"
    , "assert"
    , "break"
    , "do"
    , "dofile"
    , "dostring"
    , "else"
    , "elseif"
    , "end"
    , "error"
    , "false"
    , "for"
    , "function"
    , "getglobal"
    , "goto"
    , "if"
    , "in"
    , "local"
    , "next"
    , "nextvar"
    , "nil"
    , "not"
    , "or"
    , "print"
    , "repeat"
    , "return"
    , "setfallback"
    , "setglobal"
    , "then"
    , "tonumber"
    , "tostring"
    , "true"
    , "type"
    , "until"
    , "while"
    , "_ENV"
    , "_PS"
    ]

luaFwdRef :: forall a. Ident -> Dodo.Doc a
luaFwdRef ident = Dodo.text "local" <> Dodo.space <> codegenIdent ident

luaBinding :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
luaBinding ident b = fold
  [ Dodo.words [ Dodo.text "local", codegenIdent ident, Dodo.text "=" ]
  , Dodo.flexGroup $ Dodo.indent $ Dodo.spaceBreak <> b
  ]

luaAssign :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
luaAssign ident b = fold
  [ Dodo.words [ codegenIdent ident, Dodo.text "=" ]
  , Dodo.flexGroup $ Dodo.indent $ Dodo.spaceBreak <> b
  ]

luaAssignProp :: forall a. Ident -> Prop (Dodo.Doc a) -> Dodo.Doc a
luaAssignProp ident (Prop prop val) = fold
  [ codegenIdent ident
  , jsSquares (Dodo.text (show prop))
  , Dodo.text " ="
  , Dodo.flexGroup (Dodo.indent $ Dodo.spaceBreak <> val)
  ]

luaAccessor :: forall a. Dodo.Doc a -> String -> Dodo.Doc a
luaAccessor expr prop = expr <> jsSquares (Dodo.text (show prop))

luaIndex :: forall a. Dodo.Doc a -> Int -> Dodo.Doc a
luaIndex expr ix = expr <> jsSquares (Dodo.text (show (ix + 1)))

luaOffset :: forall a. Dodo.Doc a -> Int -> Dodo.Doc a
luaOffset expr ix = expr <> jsSquares (Dodo.text (show (ix + 2)))

luaCopyTo :: forall a. Ident -> Ident -> Dodo.Doc a -> Array (Dodo.Doc a) -> Dodo.Doc a
luaCopyTo tmp1 tmp2 val stmts = fold
  [ jsParens $ Dodo.lines
      [ Dodo.text "function (" <> codegenIdent tmp1 <> Dodo.text ")"
      , Dodo.indent $ Dodo.lines
          [ Dodo.text "local " <> codegenIdent tmp2 <> Dodo.text " = {}"
          , Dodo.text "for orig_key, orig_value in pairs(" <> codegenIdent tmp1 <> Dodo.text ")"
          , Dodo.indent $ codegenIdent tmp2 <> jsSquares (Dodo.text "orig_key") <> Dodo.text " = orig_value"
          , Dodo.text "end"
          , Dodo.lines stmts
          , Dodo.text "return " <> codegenIdent tmp2
          ]
      , Dodo.text "end"
      ]
  , jsParens val
  ]

luaBlock :: forall a. Dodo.Doc a -> Dodo.Doc a
luaBlock stmts = jsParens (luaFn mempty stmts) <> Dodo.text "()"

luaStatements :: forall a. Array (Dodo.Doc a) -> Dodo.Doc a
luaStatements = Dodo.lines

luaFn :: forall a. Array Ident -> Dodo.Doc a -> Dodo.Doc a
luaFn args stmts =
  Dodo.lines
    [ Dodo.text "function" <> jsParens (Dodo.foldWithSeparator trailingComma (codegenIdent <$> args))
    , Dodo.indent $ stmts
    , Dodo.text "end"
    ]

luaReturn :: forall a. Dodo.Doc a -> Dodo.Doc a
luaReturn doc = Dodo.flexGroup $ fold
  [ Dodo.text "return"
  , Dodo.space
  , Dodo.indent doc
  ]

luaCurriedFn :: forall f a. Foldable f => f Ident -> Dodo.Doc a -> Dodo.Doc a
luaCurriedFn = flip (foldr (luaFn <<< pure))

luaArray :: forall a. Array (Dodo.Doc a) -> Dodo.Doc a
luaArray = luaApp (Dodo.text "_PS.array")

luaRecord :: forall a. Array (Prop (Dodo.Doc a)) -> Dodo.Doc a
luaRecord = jsCurlies <<< Dodo.foldWithSeparator trailingComma <<< map luaProp

luaProp :: forall a. Prop (Dodo.Doc a) -> Dodo.Doc a
luaProp (Prop prop val) = fold
  [ jsSquares (Dodo.text (show prop))
  , Dodo.space <> Dodo.text "="
  , Dodo.indent $ Dodo.flexGroup $ Dodo.spaceBreak <> val
  ]

luaCtor :: forall a. String -> Array (Dodo.Doc a) -> Dodo.Doc a
luaCtor tag vals = jsCurlies $ Dodo.foldWithSeparator trailingComma (Array.cons (luaString tag) vals)

luaString :: forall a. String -> Dodo.Doc a
luaString = Dodo.text <<< show

luaNumber :: forall a. Number -> Dodo.Doc a
luaNumber = Dodo.text <<< show

luaInt :: forall a. Int -> Dodo.Doc a
luaInt = Dodo.text <<< show

luaChar :: forall a. Char -> Dodo.Doc a
luaChar = Dodo.text <<< show

luaBoolean :: forall a. Boolean -> Dodo.Doc a
luaBoolean = Dodo.text <<< show

luaApp :: forall f a. Foldable f => Dodo.Doc a -> f (Dodo.Doc a) -> Dodo.Doc a
luaApp a bs = a <> jsParens (Dodo.foldWithSeparator trailingComma bs)

luaCurriedApp :: forall a. Dodo.Doc a -> NonEmptyArray (Dodo.Doc a) -> Dodo.Doc a
luaCurriedApp = foldl (\a b -> a <> jsParens b)

luaIfElse :: forall f a. Foldable f => f (Tuple (Dodo.Doc a) (Dodo.Doc a)) -> Dodo.Doc a -> Dodo.Doc a
luaIfElse conds default = Dodo.lines
  [ condChain.doc
  , Monoid.guard (not (Dodo.isEmpty default)) $ Dodo.lines
      [ Dodo.text "else"
      , Dodo.indent default
      ]
  , Dodo.text "end"
  ]
  where
  condChain = foldl go { elseif: false, doc: mempty } conds
  go { elseif, doc } (Tuple cond body) =
    { elseif: true
    , doc: Dodo.lines
        [ doc
        , Dodo.flexGroup $ fold
            [ if elseif then Dodo.text "elseif" else Dodo.text "if"
            , Dodo.spaceBreak
            , Dodo.indent cond
            , Dodo.space
            , Dodo.text "then"
            ]
        , Dodo.indent body
        ]
    }

luaBranches :: forall a. Array (Tuple (Dodo.Doc a) (Dodo.Doc a)) -> Dodo.Doc a
luaBranches = Dodo.lines <<< map \(Tuple doc1 doc2) ->
  Dodo.lines
    [ Dodo.flexGroup $ fold
        [ Dodo.text "if"
        , Dodo.spaceBreak
        , Dodo.indent doc1
        , Dodo.space
        , Dodo.text "then"
        ]
    , Dodo.indent doc2
    , Dodo.text "end"
    ]

luaImport :: forall a. ModuleName -> String -> Dodo.Doc a
luaImport mn path = Dodo.words
  [ Dodo.text "local"
  , codegenModuleName mn
  , Dodo.text "="
  , Dodo.text "require" <> jsParens (Dodo.text (show path))
  ]

luaExport :: forall a. Ident -> Qualified Ident -> Prop (Dodo.Doc a)
luaExport (Ident ident) ref = Prop ident (codegenQualified codegenIdent ref)

luaModulePath :: ModuleName -> String
luaModulePath (ModuleName mn) = escapeIdent mn

luaForeignModulePath :: ModuleName -> String
luaForeignModulePath (ModuleName mn) = escapeIdent mn <> "_foreign"

luaLabel :: forall a. Ident -> Dodo.Doc a
luaLabel = Dodo.enclose (Dodo.text "::") (Dodo.text "::") <<< codegenIdent

luaGoto :: forall a. Ident -> Dodo.Doc a
luaGoto = Dodo.appendSpace (Dodo.text "goto") <<< codegenIdent

luaUndefined :: forall a. Dodo.Doc a
luaUndefined = Dodo.text "nil"

luaError :: forall a. String -> Dodo.Doc a
luaError str = luaApp (Dodo.text "error") [ luaString str ]

-- luaJoin :: forall a. Ident -> Array (Tuple Ident (Dodo.Doc a)) -> Dodo.Doc a
-- luaJoin lbl args = Dodo.lines
--   [ Dodo.lines $ uncurry luaAssign <$> args
--   , luaGoto lbl
--   ]

-- luaJoinTable :: forall a. Array Ident -> Array (Tuple Ident (Dodo.Doc a)) -> Dodo.Doc a -> Dodo.Doc a
-- luaJoinTable idents points body =
--   luaBlock
--     ( luaStatements $ fold
--         [ luaFwdRef <$> idents
--         , [ body ]
--         , uncurry Dodo.appendBreak <<< lmap luaLabel <$> points
--         ]
--     )

type BackendExprView expr r =
  ( var :: Qualified Ident -> expr
  , litInt :: Int -> expr
  , litNumber :: Number -> expr
  , litString :: String -> expr
  , litChar :: Char -> expr
  , litBoolean :: Boolean -> expr
  , litArray :: Array expr -> expr
  , litRecord :: Array (Prop expr) -> expr
  , app :: expr -> expr -> expr
  , abs :: Ident -> expr -> expr
  , accessor :: expr -> BackendAccessor -> expr
  , update :: expr -> Array (Prop expr) -> expr
  , ctor :: Ident -> Array Ident -> expr
  , letRec :: Array (Tuple Ident expr) -> expr -> expr
  , letIn :: Ident -> expr -> expr -> expr
  , branch :: Array (Tuple expr expr) -> expr
  , test :: expr -> BackendGuard -> expr
  , fail :: String -> expr
  | r
  )

buildBackendExpr :: { | BackendExprView BackendExpr () }
buildBackendExpr =
  { var: Var
  , litInt: Lit <<< LitInt
  , litNumber: Lit <<< LitNumber
  , litString: Lit <<< LitString
  , litChar: Lit <<< LitChar
  , litBoolean: Lit <<< LitBoolean
  , litArray: Lit <<< LitArray
  , litRecord: Lit <<< LitRecord
  , app: \a b -> App a (NonEmptyArray.singleton b)
  , abs: \a b -> Abs (NonEmptyArray.singleton a) b
  , accessor: Accessor
  , update: Update
  , ctor: CtorDef
  , letRec: LetRec
  , letIn: Let
  , branch: Branch
  , test: Test
  , fail: Fail
  }

monoidalBackendView :: forall a. Monoid a => { | BackendExprView a () }
monoidalBackendView =
  { var: mempty
  , litInt: mempty
  , litNumber: mempty
  , litString: mempty
  , litChar: mempty
  , litBoolean: mempty
  , litArray: fold
  , litRecord: foldMap propValue
  , app: append
  , abs: \_ a -> a
  , accessor: \a _ -> a
  , update: \a bs -> a <> foldMap propValue bs
  , ctor: mempty
  , letRec: \as b -> foldMap snd as <> b
  , letIn: \_ a b -> a <> b
  , branch: foldMap (uncurry append)
  , test: \a _ -> a
  , fail: mempty
  }

analyzeBackendExpr :: { | BackendExprView BackendAnalysis () }
analyzeBackendExpr = monoidalBackendView
  { var = used { tailCall: false }
  , abs = \_ a -> Capture a
  , letRec = IntroRec
  , letIn = Intro
  }