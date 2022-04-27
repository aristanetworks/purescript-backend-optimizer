module Backend2 where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.RWS (RWST, ask, evalRWST, state)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function (applyFlipped, on)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Newtype (class Newtype, over, un)
import Data.Set as Set
import Data.String as String
import Data.Traversable (class Foldable, class Traversable, fold, foldMap, foldl, foldlDefault, foldr, foldrDefault, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Meta(..), Module(..), ModuleName(..), Prop(..), Qualified(..), ReExport(..))

newtype Usage = Usage
  { count :: Int
  , captured :: Boolean
  }

derive instance Newtype Usage _

instance Semigroup Usage where
  append (Usage a) (Usage b) = Usage
    { count: a.count + b.count
    , captured: a.captured || b.captured
    }

data Complexity = Trivial | Deref | NonTrivial

derive instance Eq Complexity
derive instance Ord Complexity

instance Semigroup Complexity where
  append = case _, _ of
    Trivial, a -> a
    b, Trivial -> b
    Deref, a -> a
    b, Deref -> b
    _, _ -> NonTrivial

instance Monoid Complexity where
  mempty = Trivial

newtype BackendAnalysisSummary = BackendAnalysisSummary
  { usages :: Map Ident Usage
  , size :: Int
  , complexity :: Complexity
  }

instance Semigroup BackendAnalysisSummary where
  append (BackendAnalysisSummary a) (BackendAnalysisSummary b) = BackendAnalysisSummary
    { usages: Map.unionWith append a.usages b.usages
    , size: a.size + b.size
    , complexity: a.complexity <> b.complexity
    }

instance Monoid BackendAnalysisSummary where
  mempty = BackendAnalysisSummary
    { usages: Map.empty
    , size: 0
    , complexity: Trivial
    }

data InlineData = NoInline | Inline BackendExpr

type BackendInlineData = Map (Qualified Ident) InlineData

data BackendExpr = BackendExpr (BackendExprView BackendExpr) BackendInlineData BackendAnalysisSummary

data BackendExprView a
  = Var (Qualified Ident)
  -- | Local (Maybe Ident) Lvl
  | Lit (Literal a)
  | App a (NonEmptyArray a)
  | Abs (NonEmptyArray Ident) a
  | Accessor a BackendAccessor
  | Update a (Array (Prop a))
  | CtorDef Ident (Array Ident)
  | LetRec (Array (Tuple Ident a)) a
  | Let Ident a a
  | Branch (Array (Pair a))
  | Test a BackendGuard
  | Fail String

newtype Lvl = Lvl Int

derive newtype instance Eq Lvl
derive newtype instance Ord Lvl

data Pair a = Pair a a

derive instance Functor Pair

instance Foldable Pair where
  foldl f acc (Pair a b) = f (f acc a) b
  foldr f acc (Pair a b) = f a (f b acc)
  foldMap f (Pair a b) = f a <> f b

instance Traversable Pair where
  sequence a = sequenceDefault a
  traverse f (Pair a b) = Pair <$> f a <*> f b

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

derive instance Functor BackendExprView

instance Foldable BackendExprView where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    Lit lit ->
      case lit of
        LitArray as -> foldMap f as
        LitRecord as -> foldMap (foldMap f) as
        _ -> mempty
    App a bs -> f a <> foldMap f bs
    Abs _ b -> f b
    Accessor a _ -> f a
    Update a bs -> f a <> foldMap (foldMap f) bs
    LetRec as b -> foldMap (foldMap f) as <> f b
    Let _ b c -> f b <> f c
    Branch as -> foldMap (foldMap f) as
    Test a _ -> f a
    _ -> mempty

instance Traversable BackendExprView where
  sequence a = sequenceDefault a
  traverse f = case _ of
    Var a ->
      pure (Var a)
    Lit lit ->
      case lit of
        LitInt a -> pure (Lit (LitInt a))
        LitNumber a -> pure (Lit (LitNumber a))
        LitString a -> pure (Lit (LitString a))
        LitChar a -> pure (Lit (LitChar a))
        LitBoolean a -> pure (Lit (LitBoolean a))
        LitArray as -> Lit <<< LitArray <$> traverse f as
        LitRecord as -> Lit <<< LitRecord <$> traverse (traverse f) as
    App a bs ->
      App <$> f a <*> traverse f bs
    Abs as b ->
      Abs as <$> f b
    Accessor a b ->
      flip Accessor b <$> f a
    Update a bs ->
      Update <$> f a <*> traverse (traverse f) bs
    CtorDef a bs ->
      pure (CtorDef a bs)
    LetRec as b ->
      LetRec <$> traverse (traverse f) as <*> f b
    Let a b c ->
      Let a <$> f b <*> f c
    Branch as ->
      Branch <$> traverse (traverse f) as
    Test a b ->
      flip Test b <$> f a
    Fail a ->
      pure (Fail a)

inline :: forall f. Foldable f => f (Tuple (Qualified Ident) InlineData) -> BackendInlineData -> BackendExpr -> BackendExpr
inline new inlining (BackendExpr analysis inlining' expr) =
  BackendExpr analysis (Map.union inlining' (foldr (uncurry Map.insert) inlining new)) expr

bound :: forall f. Foldable f => f Ident -> BackendAnalysisSummary -> BackendAnalysisSummary
bound idents (BackendAnalysisSummary { size, usages, complexity }) =
  BackendAnalysisSummary { size, usages: foldr Map.delete usages idents, complexity }

used :: Qualified Ident -> BackendAnalysisSummary
used (Qualified mn ident) = case mn of
  Just _ -> mempty
  Nothing -> BackendAnalysisSummary
    { usages: Map.singleton ident (Usage { count: 1, captured: false })
    , size: 0
    , complexity: Trivial
    }

bump :: BackendAnalysisSummary -> BackendAnalysisSummary
bump (BackendAnalysisSummary { usages, size, complexity }) =
  BackendAnalysisSummary { usages, size: size + 1, complexity }

complex :: Complexity -> BackendAnalysisSummary -> BackendAnalysisSummary
complex complexity (BackendAnalysisSummary { usages, size }) =
  BackendAnalysisSummary { usages, size, complexity }

capture :: BackendAnalysisSummary -> BackendAnalysisSummary
capture (BackendAnalysisSummary { usages, size, complexity }) =
  BackendAnalysisSummary { usages: over Usage _ { captured = true } <$> usages, size, complexity }

analysisOf :: BackendExpr -> BackendAnalysisSummary
analysisOf (BackendExpr _ _ analysis) = analysis

analyze :: BackendExprView BackendExpr -> BackendAnalysisSummary
analyze expr = case expr of
  Var ident ->
    bump (used ident)
  Let ident a b ->
    bump (complex NonTrivial (analysisOf a <> bound [ ident ] (analysisOf b)))
  LetRec as b ->
    bump (complex NonTrivial (bound (fst <$> as) (foldMap (analysisOf <<< snd) as <> analysisOf b)))
  Abs _ _ ->
    complex NonTrivial $ capture $ analyzeDefault expr
  Update _ _ ->
    complex NonTrivial $ analyzeDefault expr
  CtorDef _ _ ->
    complex NonTrivial $ analyzeDefault expr
  Branch _ ->
    complex NonTrivial $ analyzeDefault expr
  Test _ _ ->
    complex NonTrivial $ analyzeDefault expr
  Fail _ ->
    complex NonTrivial $ analyzeDefault expr
  App _ _ ->
    complex NonTrivial $ analyzeDefault expr
  Accessor _ _ ->
    complex Deref $ analyzeDefault expr
  Lit _ ->
    analyzeDefault expr

analyzeDefault :: BackendExprView BackendExpr -> BackendAnalysisSummary
analyzeDefault = bump <<< foldMap analysisOf

build :: BackendExprView BackendExpr -> BackendExpr
build expr = BackendExpr expr Map.empty (analyze expr)

view :: BackendExpr -> BackendExprView BackendExpr
view (BackendExpr expr inlining _) = case expr of
  Var ident
    | Just (Inline val) <- Map.lookup ident inlining ->
        view val
    | otherwise ->
        expr
  Let ident a b -> do
    let BackendAnalysisSummary s1 = analysisOf a
    let BackendAnalysisSummary s2 = analysisOf b
    case Map.lookup ident s2.usages of
      Just (Usage { captured, count })
        | not captured && (count == 1 || (s1.complexity <= Deref && s1.size < 5)) ->
            view (inline [ Tuple (Qualified Nothing ident) (Inline (inline [] inlining a)) ] inlining b)
      _ ->
        Let ident
          (inline [] inlining a)
          (inline [ Tuple (Qualified Nothing ident) NoInline ] inlining b)
  LetRec as b -> do
    let bindings = (\(Tuple id _) -> Tuple (Qualified Nothing id) NoInline) <$> as
    let f = inline bindings inlining
    LetRec (map f <$> as) (f b)
  Abs idents b -> do
    let bindings = (\id -> Tuple (Qualified Nothing id) NoInline) <$> idents
    Abs idents (inline bindings inlining b)
  Update _ _ ->
    inlineDefault inlining expr
  Branch _ ->
    inlineDefault inlining expr
  Test _ _ ->
    inlineDefault inlining expr
  Accessor _ _ ->
    inlineDefault inlining expr
  App _ _ ->
    inlineDefault inlining expr
  Lit _ ->
    inlineDefault inlining expr
  CtorDef _ _ ->
    expr
  Fail _ ->
    expr

inlineDefault :: BackendInlineData -> BackendExprView BackendExpr -> BackendExprView BackendExpr
inlineDefault inlining expr = inline [] inlining <$> expr

type BackendBindingGroup =
  { recursive :: Boolean
  , bindings :: Array (Tuple Ident BackendExpr)
  }

newtype BackendModule = BackendModule
  { imports :: Array (Tuple ModuleName String)
  , bindings :: Array BackendBindingGroup
  , exports :: Array (Tuple Ident (Qualified Ident))
  }

type BackendEnv =
  { coreFnModules :: Map ModuleName (Module Ann)
  , backendModules :: Map ModuleName BackendModule
  , currentModule :: ModuleName
  }

type BackendState =
  { fresh :: Int
  }

type BackendM = RWST BackendEnv Unit BackendState Identity

runBackendM :: forall a. BackendEnv -> Int -> BackendM a -> a
runBackendM env fresh m = fst $ un Identity $ evalRWST m env { fresh }

tmpIdent :: BackendM Ident
tmpIdent = state \st -> Tuple (Ident ("$t" <> show st.fresh)) (st { fresh = st.fresh + 1 })

toBackendModule :: Module Ann -> BackendM BackendModule
toBackendModule (Module mod@{ name: ModuleName this }) = do
  bindings <- toBackendTopLevelBindingGroups mod.decls
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
            , bindings: map (\ident -> Tuple ident (build (Var (Qualified (Just foreignModuleName) ident)))) mod.foreign
            }
          ]
        , bindings
        ]
    , exports: fold
        [ map (\a -> Tuple a (Qualified Nothing a)) mod.exports
        , map (\(ReExport mn a) -> Tuple a (Qualified (Just mn) a)) mod.reExports
        ]
    }

toBackendTopLevelBindingGroups :: Array (Bind Ann) -> BackendM (Array BackendBindingGroup)
toBackendTopLevelBindingGroups binds = do
  binds' <- traverse toBackendTopLevelBindingGroup binds
  pure $ (\as -> { recursive: (NonEmptyArray.head as).recursive, bindings: _.bindings =<< NonEmptyArray.toArray as }) <$>
    Array.groupBy ((&&) `on` (not <<< _.recursive)) binds'

toBackendTopLevelBindingGroup :: Bind Ann -> BackendM BackendBindingGroup
toBackendTopLevelBindingGroup = case _ of
  Rec bindings ->
    { recursive: true, bindings: _ } <$> traverse toBackendBinding bindings
  NonRec binding ->
    { recursive: false, bindings: _ } <<< pure <$> toBackendBinding binding

data PatternStk
  = PatBinder (Binder Ann) PatternStk
  | PatPush BackendAccessor PatternStk
  | PatPop PatternStk
  | PatNil

toBackendExpr :: Expr Ann -> BackendM BackendExpr
toBackendExpr = case _ of
  ExprVar _ qi@(Qualified mn ident) -> do
    { currentModule } <- ask
    let
      qi'
        | mn == Just currentModule = Qualified Nothing ident
        | otherwise = qi
    pure (build (Var qi'))
  ExprLit _ lit ->
    build <<< Lit <$> traverse toBackendExpr lit
  ExprConstructor _ _ name fields ->
    pure (build (CtorDef name fields))
  ExprAccessor _ a field ->
    build <<< flip Accessor (GetProp field) <$> toBackendExpr a
  ExprUpdate _ a bs ->
    (\x y -> build (Update x y))
      <$> toBackendExpr a
      <*> traverse (traverse toBackendExpr) bs
  ExprAbs _ arg body -> do
    let Tuple args expr = flattenAbs (NonEmptyArray.singleton arg) body
    build <<< Abs args <$> toBackendExpr expr
  ExprApp _ a b
    | ExprVar (Ann { meta: Just IsNewtype }) _ <- a ->
        toBackendExpr b
    | otherwise -> do
        let Tuple fn args = flattenApp (NonEmptyArray.singleton b) a
        (\x y -> build (App x y))
          <$> toBackendExpr fn
          <*> traverse toBackendExpr args
  ExprLet _ binds body ->
    foldr go (toBackendExpr body) binds
    where
    go bind' next = case bind' of
      Rec bindings ->
        (\x y -> build (LetRec x y))
          <$> traverse toBackendBinding bindings
          <*> next
      NonRec binding ->
        (\(Tuple ident x) y -> build (Let ident x y))
          <$> toBackendBinding binding
          <*> next
  ExprCase _ exprs alts -> do
    scrutinees <- traverse (\e -> flip Tuple (toBackendExpr e) <$> tmpIdent) exprs
    let branches = foldr (lift2 mergeBranches) patternFail $ goAlt (fst <$> scrutinees) <$> alts
    foldr (\(Tuple x y) z -> make (Let x y z)) branches scrutinees
  where
  mergeBranches :: BackendExpr -> BackendExpr -> BackendExpr 
  mergeBranches = case _, _ of
    BackendExpr (Branch bs1) _ a1, BackendExpr (Branch bs2) _ a2
      | Just (Pair (BackendExpr (Lit (LitBoolean true)) _ _) _) <- Array.last bs1 ->
          BackendExpr (Branch bs1) Map.empty a1
      | otherwise ->
          BackendExpr (Branch (bs1 <> bs2)) Map.empty (a1 <> a2)
    BackendExpr (Branch bs1) _ _, nonBranch ->
      build (Branch (Array.snoc bs1 (Pair (build (Lit (LitBoolean true))) nonBranch)))
    nonBranch, _ ->
      nonBranch

  flattenAbs :: forall a. NonEmptyArray Ident -> Expr a -> Tuple (NonEmptyArray Ident) (Expr a)
  flattenAbs args = case _ of
    ExprAbs _ ident b ->
      flattenAbs (NonEmptyArray.snoc args ident) b
    expr ->
      Tuple args expr

  flattenApp :: forall a. NonEmptyArray (Expr a) -> Expr a -> Tuple (Expr a) (NonEmptyArray (Expr a))
  flattenApp bs = case _ of
    ExprApp _ a b ->
      flattenApp (NonEmptyArray.cons b bs) a
    expr ->
      Tuple expr bs

  goAlt :: Array Ident -> CaseAlternative Ann -> BackendM BackendExpr
  goAlt idents (CaseAlternative binders branch) =
    goBinders
      ( \renames -> foldr
          ( \(Tuple a b) ->
              make <<< Let a (make (Var (Qualified Nothing b)))
          )
          (goCaseGuard branch)
          renames
      )
      List.Nil
      List.Nil
      (List.fromFoldable idents)
      (foldr (\b s -> PatBinder b (PatPop s)) PatNil binders)

  goCaseGuard :: CaseGuard Ann -> BackendM BackendExpr
  goCaseGuard = case _ of
    Unconditional expr ->
      toBackendExpr expr
    Guarded gs ->
      build <<< Branch <$> traverse (\(Guard a b) -> Pair <$> toBackendExpr a <*> toBackendExpr b) gs

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
          goBinders k (List.Cons (make <<< Let tmp (make (Accessor (make (Var (Qualified Nothing id))) accessor))) ks) store (List.Cons tmp stk) next
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

  patternFail :: BackendM BackendExpr
  patternFail =
    make (Branch [ Pair (make (Lit (LitBoolean true))) (make (Fail "Failed pattern match")) ])

  makeGuard :: Ident -> BackendGuard -> BackendM BackendExpr -> BackendM BackendExpr
  makeGuard id g inner =
    make $ Branch [ Pair (make (Test (make (Var (Qualified Nothing id))) g)) inner ]

  make :: BackendExprView (BackendM BackendExpr) -> BackendM BackendExpr
  make = map build <<< sequence

toBackendBinding :: Binding Ann -> BackendM (Tuple Ident BackendExpr)
toBackendBinding (Binding _ ident expr) = Tuple ident <$> toBackendExpr expr

luaCodegenModule :: forall a. BackendModule -> Dodo.Doc a
luaCodegenModule (BackendModule mod) =
  Dodo.lines
    [ Dodo.lines $ uncurry luaImport <$> mod.imports
    , Dodo.lines $ luaCodegenBindingGroup =<< mod.bindings
    , Dodo.words
        [ Dodo.text "return"
        , luaRecord (map (uncurry luaExport) mod.exports)
        ]
    ]

luaCodegenExpr :: forall a. BackendExpr -> Dodo.Doc a
luaCodegenExpr a = luaCodegenExprView (view a)

luaCodegenExprView :: forall a. BackendExprView BackendExpr -> Dodo.Doc a
luaCodegenExprView = case _ of
  Var (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) ->
    luaUndefined
  Var var ->
    luaCodegenQualified luaCodegenIdent var
  Lit lit ->
    luaCodegenLit lit
  App a bs ->
    luaCurriedApp (luaCodegenExpr a) (luaCodegenExpr <$> bs)
  Abs idents body
    | [ Ident "$__unused" ] <- NonEmptyArray.toArray idents ->
        luaFn [] (luaCodegenBlockStatements (view body))
    | otherwise ->
        luaCurriedFn idents (luaCodegenBlockStatements (view body))
  Accessor a prop ->
    luaCodegenAccessor (luaCodegenExpr a) prop
  Update a props ->
    luaUpdate (luaCodegenExpr a) (map luaCodegenExpr <$> props)
  CtorDef (Ident tag) fields ->
    luaCurriedFn fields (luaCtor tag (luaCodegenIdent <$> fields))
  Test a b ->
    luaCodegenTest (luaCodegenExpr a) b
  Fail str ->
    luaError str
  expr@(Branch _) ->
    luaCodegenBlock expr
  expr@(LetRec _ _) ->
    luaCodegenBlock expr
  expr@(Let _ _ _) ->
    luaCodegenBlock expr

luaCodegenLit :: forall a. Literal BackendExpr -> Dodo.Doc a
luaCodegenLit = case _ of
  LitInt n ->
    luaInt n
  LitNumber n ->
    luaNumber n
  LitString str ->
    luaString str
  LitChar ch ->
    luaChar ch
  LitBoolean bool ->
    Dodo.text (show bool)
  LitArray as ->
    luaArray (luaCodegenExpr <$> as)
  LitRecord props ->
    luaRecord (map luaCodegenExpr <$> props)

luaCodegenBlock :: forall a. BackendExprView BackendExpr -> Dodo.Doc a
luaCodegenBlock a = luaBlock (luaCodegenBlockStatements a)

luaCodegenBlockBranches :: forall a. Array (Pair BackendExpr) -> Dodo.Doc a
luaCodegenBlockBranches = (\bs -> luaBranches (go <$> bs))
  where
  go :: Pair BackendExpr -> Tuple (Dodo.Doc a) (Dodo.Doc a)
  go = map view >>> case _ of
    Pair a (Branch bs) ->
      Tuple (luaCodegenExprView a) (luaCodegenBlockBranches bs)
    Pair a b ->
      Tuple (luaCodegenExprView a) (luaCodegenBlockStatements b)

luaCodegenBlockStatements :: forall a. BackendExprView BackendExpr -> Dodo.Doc a
luaCodegenBlockStatements = luaStatements <<< go []
  where
  go :: Array (Dodo.Doc a) -> BackendExprView BackendExpr -> Array (Dodo.Doc a)
  go acc = case _ of
    LetRec bindings body -> do
      let lines = luaCodegenBindingGroup { recursive: true, bindings }
      go (acc <> lines) (view body)
    Let ident expr body -> do
      let lines = luaCodegenBindingGroup { recursive: false, bindings: [ Tuple ident expr ] }
      go (acc <> lines) (view body)
    Branch bs ->
      Array.snoc acc (luaCodegenBlockBranches bs)
    expr@(Fail _) ->
      Array.snoc acc (luaCodegenExprView expr)
    expr ->
      Array.snoc acc (luaReturn (luaCodegenExprView expr))

luaCodegenBindingGroup :: forall a. BackendBindingGroup -> Array (Dodo.Doc a)
luaCodegenBindingGroup { recursive, bindings }
  | recursive = do
      let fwdRefs = luaFwdRef <<< fst <$> bindings
      fwdRefs <> map (\(Tuple ident b) -> luaAssign ident (luaCodegenExpr b)) bindings
  | otherwise =
      map (\(Tuple ident b) -> luaBinding ident (luaCodegenExpr b)) bindings

luaCodegenTest :: forall a. Dodo.Doc a -> BackendGuard -> Dodo.Doc a
luaCodegenTest lhs = case _ of
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

luaCodegenAccessor :: forall a. Dodo.Doc a -> BackendAccessor -> Dodo.Doc a
luaCodegenAccessor lhs = case _ of
  GetProp p ->
    luaAccessor lhs p
  GetIndex n ->
    luaIndex lhs n
  GetOffset n ->
    luaOffset lhs n

luaCodegenIdent :: forall a. Ident -> Dodo.Doc a
luaCodegenIdent (Ident a) = Dodo.text (luaEscapeIdent a)

luaCodegenQualified :: forall a b. (a -> Dodo.Doc b) -> Qualified a -> Dodo.Doc b
luaCodegenQualified codegenInner (Qualified qual inner) = case qual of
  Nothing -> codegenInner inner
  Just mn -> luaCodegenModuleName mn <> Dodo.text "." <> codegenInner inner

luaCodegenModuleName :: forall a. ModuleName -> Dodo.Doc a
luaCodegenModuleName (ModuleName mn) = Dodo.text (luaEscapeIdent mn)

luaEscapeIdent :: String -> String
luaEscapeIdent = escapeReserved
  where
  escapeReserved str
    | Set.member str reservedNames =
        str <> "_r"
    | otherwise =
        escapeSpecial str

  escapeSpecial =
    String.replaceAll (String.Pattern "_") (String.Replacement "_u")
      >>> String.replaceAll (String.Pattern "'") (String.Replacement "_p")
      >>> String.replaceAll (String.Pattern ".") (String.Replacement "_")
      >>> String.replaceAll (String.Pattern "$") (String.Replacement "__")

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
luaFwdRef ident = Dodo.text "local" <> Dodo.space <> luaCodegenIdent ident

luaBinding :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
luaBinding ident b = fold
  [ Dodo.words [ Dodo.text "local", luaCodegenIdent ident, Dodo.text "=" ]
  , Dodo.flexGroup $ Dodo.indent $ Dodo.spaceBreak <> b
  ]

luaAssign :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
luaAssign ident b = fold
  [ Dodo.words [ luaCodegenIdent ident, Dodo.text "=" ]
  , Dodo.flexGroup $ Dodo.indent $ Dodo.spaceBreak <> b
  ]

luaAssignProp :: forall a. Ident -> Prop (Dodo.Doc a) -> Dodo.Doc a
luaAssignProp ident (Prop prop val) = fold
  [ luaCodegenIdent ident
  , Dodo.Common.jsSquares (Dodo.text (show prop))
  , Dodo.text " ="
  , Dodo.flexGroup (Dodo.indent $ Dodo.spaceBreak <> val)
  ]

luaAccessor :: forall a. Dodo.Doc a -> String -> Dodo.Doc a
luaAccessor expr prop = expr <> Dodo.Common.jsSquares (Dodo.text (show prop))

luaIndex :: forall a. Dodo.Doc a -> Int -> Dodo.Doc a
luaIndex expr ix = expr <> Dodo.Common.jsSquares (Dodo.text (show (ix + 1)))

luaOffset :: forall a. Dodo.Doc a -> Int -> Dodo.Doc a
luaOffset expr ix = expr <> Dodo.Common.jsSquares (Dodo.text (show (ix + 2)))

luaUpdate :: forall a. Dodo.Doc a -> Array (Prop (Dodo.Doc a)) -> Dodo.Doc a
luaUpdate rec props = luaApp (Dodo.text "_PS.record_update") [ rec, luaRecord props ]

luaBlock :: forall a. Dodo.Doc a -> Dodo.Doc a
luaBlock stmts = Dodo.Common.jsParens (luaFn mempty stmts) <> Dodo.text "()"

luaStatements :: forall a. Array (Dodo.Doc a) -> Dodo.Doc a
luaStatements = Dodo.lines

luaFn :: forall a. Array Ident -> Dodo.Doc a -> Dodo.Doc a
luaFn args stmts =
  Dodo.lines
    [ Dodo.text "function" <> Dodo.Common.jsParens
        (Dodo.foldWithSeparator Dodo.Common.trailingComma (luaCodegenIdent <$> args))
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
luaRecord = Dodo.Common.jsCurlies <<< Dodo.foldWithSeparator Dodo.Common.trailingComma <<< map luaProp

luaProp :: forall a. Prop (Dodo.Doc a) -> Dodo.Doc a
luaProp (Prop prop val) = fold
  [ Dodo.Common.jsSquares (Dodo.text (show prop))
  , Dodo.space <> Dodo.text "="
  , Dodo.indent $ Dodo.flexGroup $ Dodo.spaceBreak <> val
  ]

luaCtor :: forall a. String -> Array (Dodo.Doc a) -> Dodo.Doc a
luaCtor tag vals = Dodo.Common.jsCurlies $
  Dodo.foldWithSeparator Dodo.Common.trailingComma (Array.cons (luaString tag) vals)

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
luaApp a bs = a <> Dodo.Common.jsParens (Dodo.foldWithSeparator Dodo.Common.trailingComma bs)

luaCurriedApp :: forall a. Dodo.Doc a -> NonEmptyArray (Dodo.Doc a) -> Dodo.Doc a
luaCurriedApp = foldl (\a b -> a <> Dodo.Common.jsParens b)

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
  , luaCodegenModuleName mn
  , Dodo.text "="
  , Dodo.text "require" <> Dodo.Common.jsParens (Dodo.text (show path))
  ]

luaExport :: forall a. Ident -> Qualified Ident -> Prop (Dodo.Doc a)
luaExport (Ident ident) ref = Prop ident (luaCodegenQualified luaCodegenIdent ref)

luaModulePath :: ModuleName -> String
luaModulePath (ModuleName mn) = luaEscapeIdent mn

luaForeignModulePath :: ModuleName -> String
luaForeignModulePath (ModuleName mn) = luaEscapeIdent mn <> "_foreign"

luaLabel :: forall a. Ident -> Dodo.Doc a
luaLabel = Dodo.enclose (Dodo.text "::") (Dodo.text "::") <<< luaCodegenIdent

luaGoto :: forall a. Ident -> Dodo.Doc a
luaGoto = Dodo.appendSpace (Dodo.text "goto") <<< luaCodegenIdent

luaUndefined :: forall a. Dodo.Doc a
luaUndefined = Dodo.text "nil"

luaError :: forall a. String -> Dodo.Doc a
luaError str = luaApp (Dodo.text "error") [ luaString str ]