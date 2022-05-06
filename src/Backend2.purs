module Backend2 where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.RWS (ask)
import Data.Array (findMap)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid as Monoid
import Data.Newtype (class Newtype, over, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (class Foldable, class Traversable, fold, foldMap, foldl, foldlDefault, foldr, foldrDefault, mapAccumL, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Meta(..), Module(..), ModuleName(..), Prop(..), Qualified(..), ReExport(..))

newtype Usage = Usage
  { count :: Int
  , captured :: Boolean
  , arities :: Set Int
  }

derive instance Newtype Usage _

instance Semigroup Usage where
  append (Usage a) (Usage b) = Usage
    { count: a.count + b.count
    , captured: a.captured || b.captured
    , arities: Set.union a.arities b.arities
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

newtype BackendAnalysis = BackendAnalysis
  { usages :: Map Level Usage
  , size :: Int
  , complexity :: Complexity
  }

instance Semigroup BackendAnalysis where
  append (BackendAnalysis a) (BackendAnalysis b) = BackendAnalysis
    { usages: Map.unionWith append a.usages b.usages
    , size: a.size + b.size
    , complexity: a.complexity <> b.complexity
    }

instance Monoid BackendAnalysis where
  mempty = BackendAnalysis
    { usages: Map.empty
    , size: 0
    , complexity: Trivial
    }

data BackendExpr = BackendExpr BackendInlineEnv BackendAnalysis (BackendExprView BackendExpr)

data BackendExprView a
  = Var (Qualified Ident)
  | Local (Maybe Ident) Level
  | Lit (Literal a)
  | App a (NonEmptyArray a)
  | Abs (NonEmptyArray (Tuple (Maybe Ident) Level)) a
  | Accessor a BackendAccessor
  | Update a (Array (Prop a))
  | CtorDef Ident (Array Ident)
  | LetRec Level (Array (Tuple Ident a)) a
  | Let (Maybe Ident) Level a a
  | Branch (Array (Pair a))
  | Test a BackendGuard
  | Fail String

newtype Level = Level Int

derive newtype instance Eq Level
derive newtype instance Ord Level
derive instance Newtype Level _

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
    LetRec _ as b -> foldMap (foldMap f) as <> f b
    Let _ _ b c -> f b <> f c
    Branch as -> foldMap (foldMap f) as
    Test a _ -> f a
    _ -> mempty

instance Traversable BackendExprView where
  sequence a = sequenceDefault a
  traverse f = case _ of
    Var a ->
      pure (Var a)
    Local a b ->
      pure (Local a b)
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
    LetRec lvl as b ->
      LetRec lvl <$> traverse (traverse f) as <*> f b
    Let ident lvl b c ->
      Let ident lvl <$> f b <*> f c
    Branch as ->
      Branch <$> traverse (traverse f) as
    Test a b ->
      flip Test b <$> f a
    Fail a ->
      pure (Fail a)

bound :: Level -> BackendAnalysis -> BackendAnalysis
bound level (BackendAnalysis { size, usages, complexity }) =
  BackendAnalysis { size, usages: Map.delete level usages, complexity }

used :: Level -> BackendAnalysis
used level = BackendAnalysis
  { usages: Map.singleton level (Usage { count: 1, captured: false, arities: Set.empty })
  , size: 0
  , complexity: Trivial
  }

bump :: BackendAnalysis -> BackendAnalysis
bump (BackendAnalysis { usages, size, complexity }) =
  BackendAnalysis { usages, size: size + 1, complexity }

complex :: Complexity -> BackendAnalysis -> BackendAnalysis
complex complexity (BackendAnalysis { usages, size }) =
  BackendAnalysis { usages, size, complexity }

capture :: BackendAnalysis -> BackendAnalysis
capture (BackendAnalysis { usages, size, complexity }) =
  BackendAnalysis { usages: over Usage _ { captured = true } <$> usages, size, complexity }

callArity :: Level -> Int -> BackendAnalysis -> BackendAnalysis
callArity lvl arity (BackendAnalysis { usages, size, complexity }) =
  BackendAnalysis
    { usages: Map.update (Just <<< over Usage (\us -> us { arities = Set.insert arity us.arities })) lvl usages
    , size
    , complexity
    }

analysisOf :: BackendExpr -> BackendAnalysis
analysisOf (BackendExpr _ analysis _) = analysis

analyze :: BackendExprView BackendExpr -> BackendAnalysis
analyze expr = case expr of
  Var _ ->
    bump mempty
  Local _ lvl ->
    bump (used lvl)
  Let _ lvl a b ->
    bump (complex NonTrivial (analysisOf a <> bound lvl (analysisOf b)))
  LetRec lvl as b ->
    bump (complex NonTrivial (bound lvl (foldMap (analysisOf <<< snd) as <> analysisOf b)))
  Abs args _ ->
    complex NonTrivial $ capture $ foldr (bound <<< snd) (analyzeDefault expr) args
  App (BackendExpr _ _ hd) tl ->
    case hd of
      Local _ lvl ->
        callArity lvl (NonEmptyArray.length tl) analysis
      _ ->
        analysis
    where
    analysis = complex NonTrivial $ analyzeDefault expr
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
  Accessor _ _ ->
    complex Deref $ analyzeDefault expr
  Lit _ ->
    analyzeDefault expr

analyzeDefault :: BackendExprView BackendExpr -> BackendAnalysis
analyzeDefault = bump <<< foldMap analysisOf

data VarImpl
  = ForeignImpl
  | CoreFnImpl BackendAnalysis (Expr Ann)

type BackendInlineEnv =
  { currentModule :: ModuleName
  , currentLevel :: Level
  , vars :: Map (Qualified Ident) VarImpl
  }

build :: BackendInlineEnv -> BackendExprView BackendExpr -> BackendExpr
build env = case _ of
  App (BackendExpr _ _ (App hd tl1)) tl2 ->
    build env $ App hd (tl1 <> tl2)
  Abs ids1 (BackendExpr _ _ (Abs ids2 body)) ->
    build env $ Abs (ids1 <> ids2) body
  Abs idents (BackendExpr _ _ (App f args))
    | [ Tuple _ lvl1 ] <- NonEmptyArray.toArray idents
    , [ BackendExpr _ _ (Local _ lvl2) ] <- NonEmptyArray.toArray args
    , lvl1 == lvl2 ->
        f
  expr ->
    BackendExpr env (analyze expr) expr

buildM :: BackendExprView BackendExpr -> BackendM BackendExpr
buildM expr env = build
  { currentModule: env.currentModule
  , currentLevel: Level env.currentLevel
  , vars: env.vars
  }
  expr

withVars :: Map (Qualified Ident) VarImpl -> BackendExpr -> BackendExpr
withVars vars (BackendExpr env analysis expr) =
  BackendExpr (env { vars = vars }) analysis $ map (withVars vars) expr

type Relevel =
  { original :: Level
  , delta :: Int
  }

relevelSub :: Level -> BackendExpr -> BackendExpr
relevelSub =
  ( \next@(Level n) expr@(BackendExpr { currentLevel: prev@(Level p) } _ _) ->
      go { original: prev, delta: n - p } next expr
  )
  where
  go origLevel newLevel@(Level lvl) (BackendExpr env _ expr) = case expr of
    Local ident refLevel
      | refLevel >= origLevel.original ->
          build (env { currentLevel = newLevel }) do
            Local ident (over Level (add origLevel.delta) refLevel)
      | otherwise ->
          build (env { currentLevel = newLevel }) do
            Local ident refLevel
    Abs args inner ->
      build (env { currentLevel = newLevel }) do
        Abs (mapWithIndex (\ix (Tuple ident _) -> Tuple ident (Level (lvl + ix))) args) do
          go origLevel (Level (lvl + NonEmptyArray.length args)) inner
    Let ident _ expr1 expr2 ->
      build (env { currentLevel = newLevel }) do
        Let ident newLevel
          (go origLevel newLevel expr1)
          (go origLevel (Level (lvl + 1)) expr2)
    LetRec _ bindings expr1 ->
      build (env { currentLevel = newLevel }) do
        LetRec newLevel
          (map (go origLevel (Level (lvl + 1))) <$> bindings)
          (go origLevel (Level (lvl + 1)) expr1)
    _ ->
      build (env { currentLevel = newLevel }) do
        go origLevel newLevel <$> expr

inlineSub :: Level -> BackendExpr -> BackendExpr -> BackendExpr
inlineSub subLevel newExpr (BackendExpr env2 _ expr) = case expr of
  Local _ refLevel | subLevel == refLevel ->
    relevelSub env2.currentLevel newExpr
  _ ->
    build env2 $ inlineSub subLevel newExpr <$> expr

review :: BackendExpr -> BackendExpr
review expr@(BackendExpr env _ _) = build env $ map review $ view expr

view :: BackendExpr -> BackendExprView BackendExpr
view (BackendExpr env _ expr) = case expr of
  Let _ lvl a b
    | a' <- review a
    , shouldInlineLet lvl a' b ->
        view $ relevelSub env.currentLevel (inlineSub lvl a' b)
  Accessor (BackendExpr env' _ (Var qi)) acc | Just expr' <- inlineVarAccessor env' qi acc ->
    view expr'
  Var qi | Just expr' <- inlineVarImpl env qi [] ->
    view expr'
  App head@(BackendExpr env' _ _) args ->
    case view head of
      Var qi | Just expr' <- inlineVarImpl env' qi (NonEmptyArray.toArray args) ->
        view expr'
      Abs idents body -> do
        let
          appliedArgs = NonEmptyArray.zip idents args
          appliedLen = NonEmptyArray.length appliedArgs
          appliedUnder = NonEmptyArray.drop appliedLen idents
          appliedOver = NonEmptyArray.drop appliedLen args
          body' = case NonEmptyArray.fromArray appliedUnder of
            Nothing ->
              body
            Just idents' -> do
              let Tuple _ lvl = NonEmptyArray.head idents'
              build (env { currentLevel = lvl }) do
                Abs idents' body
          head' = foldr
            ( \(Tuple (Tuple ident lvl) arg) next ->
                build (env { currentLevel = lvl })
                  $ Let ident lvl (relevelSub lvl arg) next
            )
            body'
            appliedArgs
          expr' = case NonEmptyArray.fromArray appliedOver of
            Nothing ->
              head'
            Just args' ->
              build env $ App head' args'
        view $ relevelSub env.currentLevel $ expr'
      other ->
        App (build env' other) args
  Accessor head@(BackendExpr env' _ _) acc ->
    case view head of
      Var qi | Just expr' <- inlineVarAccessor env' qi acc ->
        view expr'
      other ->
        Accessor (build env' other) acc
  _ ->
    expr

overInlineEnv :: (BackendInlineEnv -> BackendInlineEnv) -> BackendExpr -> BackendExpr
overInlineEnv k (BackendExpr env analysis expr) = BackendExpr (k env) analysis expr

shouldInlineLet :: Level -> BackendExpr -> BackendExpr -> Boolean
shouldInlineLet lvl a b = do
  let BackendExpr _ (BackendAnalysis s1) e1 = a
  let BackendExpr _ (BackendAnalysis s2) _ = b
  case Map.lookup lvl s2.usages of
    Nothing ->
      true
    Just (Usage { captured, count }) ->
      (s1.complexity == Trivial && s1.size < 5)
        || (not captured && (count == 1 || (s1.complexity <= Deref && s1.size < 5)))
        || (isAbs e1 && (Map.isEmpty s1.usages || s1.size < 128))

shouldInlineVar :: BackendAnalysis -> Expr Ann -> Boolean
shouldInlineVar (BackendAnalysis s) e =
  (s.complexity == Trivial && s.size < 5)
    || (s.complexity <= Deref && s.size < 5)
    || (isCoreFnAbs e && s.size < 128)

inlineVarImpl :: BackendInlineEnv -> Qualified Ident -> Array BackendExpr -> Maybe BackendExpr
inlineVarImpl env qi _ = Map.lookup qi env.vars >>= go
  where
  go = case _ of
    CoreFnImpl analysis (ExprApp _ (ExprVar (Ann { meta: Just IsNewtype }) _) expr) ->
      go (CoreFnImpl analysis expr)
    CoreFnImpl analysis expr | shouldInlineVar analysis expr ->
      Just $ toBackendExpr expr
        { currentLevel: unwrap env.currentLevel
        , currentModule: env.currentModule
        , toLevel: Map.empty
        , vars: env.vars
        }
    _ ->
      Nothing

inlineVarAccessor :: BackendInlineEnv -> Qualified Ident -> BackendAccessor -> Maybe BackendExpr
inlineVarAccessor env qi accessor = Map.lookup qi env.vars >>= go
  where
  go = case _ of
    CoreFnImpl analysis (ExprApp _ (ExprVar (Ann { meta: Just IsNewtype }) _) expr) ->
      go (CoreFnImpl analysis expr)
    CoreFnImpl _ (ExprLit _ (LitRecord props)) | GetProp prop <- accessor ->
      props
        # findMap (\(Prop prop' val) -> val <$ guard (prop == prop'))
        # map
            ( \expr' ->
                toBackendExpr expr'
                  { currentLevel: unwrap env.currentLevel
                  , currentModule: env.currentModule
                  , toLevel: Map.empty
                  , vars: env.vars
                  }
            )
    _ ->
      Nothing

isAbs :: forall a. BackendExprView a -> Boolean
isAbs = case _ of
  Abs _ _ -> true
  _ -> false

isCoreFnAbs :: Expr Ann -> Boolean
isCoreFnAbs = case _ of
  ExprAbs _ _ _ -> true
  _ -> false

type BackendBindingGroup b =
  { recursive :: Boolean
  , bindings :: Array (Tuple Ident b)
  }

newtype BackendModule = BackendModule
  { name :: ModuleName
  , imports :: Array (Tuple ModuleName String)
  , bindings :: Array (BackendBindingGroup (Tuple BackendExpr (Maybe (Expr Ann))))
  , exports :: Array (Tuple Ident (Qualified Ident))
  }

type BackendEnv =
  { currentModule :: ModuleName
  , currentLevel :: Int
  , toLevel :: Map Ident Level
  , vars :: Map (Qualified Ident) VarImpl
  }

type BackendM = Function BackendEnv

levelUp :: forall a. BackendM a -> BackendM a
levelUp f env = f (env { currentLevel = env.currentLevel + 1 })

intro :: forall f a. Foldable f => f Ident -> Level -> BackendM a -> BackendM a
intro ident lvl f env = f
  ( env
      { currentLevel = env.currentLevel + 1
      , toLevel = foldr (flip Map.insert lvl) env.toLevel ident
      }
  )

currentLevel :: BackendM Level
currentLevel env = Level env.currentLevel

currentInlineEnv :: BackendM BackendInlineEnv
currentInlineEnv env =
  { currentModule: env.currentModule
  , currentLevel: Level env.currentLevel
  , vars: env.vars
  }

toBackendModule :: Module Ann -> BackendM BackendModule
toBackendModule (Module mod@{ name: ModuleName this }) = do
  bindings <- toBackendTopLevelBindingGroups mod.decls
  let foreignModuleName = ModuleName (this <> "$foreign")
  env <- currentInlineEnv
  pure $ BackendModule
    { name: mod.name
    , imports: fold
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
            , bindings: map
                ( \ident ->
                    Tuple ident (Tuple (build env (Var (Qualified (Just foreignModuleName) ident))) Nothing)
                )
                mod.foreign
            }
          ]
        , bindings
        ]
    , exports: fold
        [ map (\a -> Tuple a (Qualified Nothing a)) mod.exports
        , map (\(ReExport mn a) -> Tuple a (Qualified (Just mn) a)) mod.reExports
        ]
    }

toBackendTopLevelBindingGroups :: Array (Bind Ann) -> BackendM (Array (BackendBindingGroup (Tuple BackendExpr (Maybe (Expr Ann)))))
toBackendTopLevelBindingGroups binds = do
  binds' <- traverse toBackendTopLevelBindingGroup binds
  pure $ (\as -> { recursive: (NonEmptyArray.head as).recursive, bindings: _.bindings =<< NonEmptyArray.toArray as }) <$>
    Array.groupBy ((&&) `on` (not <<< _.recursive)) binds'

toBackendTopLevelBindingGroup :: Bind Ann -> BackendM (BackendBindingGroup (Tuple BackendExpr (Maybe (Expr Ann))))
toBackendTopLevelBindingGroup = case _ of
  Rec bindings ->
    { recursive: true, bindings: _ } <$> traverse go bindings
  NonRec binding ->
    { recursive: false, bindings: _ } <<< pure <$> go binding
  where
  go (Binding _ ident expr) =
    Tuple ident <<< flip Tuple (Just expr) <$> toBackendExpr expr

data PatternStk
  = PatBinder (Binder Ann) PatternStk
  | PatPush BackendAccessor PatternStk
  | PatPop PatternStk
  | PatNil

toBackendExpr :: Expr Ann -> BackendM BackendExpr
toBackendExpr = case _ of
  ExprVar _ qi@(Qualified mn ident) -> do
    { currentModule, toLevel } <- ask
    let
      qi'
        | mn == Just currentModule = Qualified Nothing ident
        | otherwise = qi
    case qi' of
      Qualified Nothing _ | Just lvl <- Map.lookup ident toLevel ->
        buildM (Local (Just ident) lvl)
      _ ->
        buildM (Var qi')
  ExprLit _ lit ->
    buildM <<< Lit =<< traverse toBackendExpr lit
  ExprConstructor _ _ name fields ->
    buildM (CtorDef name fields)
  ExprAccessor _ a field ->
    buildM <<< flip Accessor (GetProp field) =<< toBackendExpr a
  ExprUpdate _ a bs ->
    join $ (\x y -> buildM (Update x y))
      <$> toBackendExpr a
      <*> traverse (traverse toBackendExpr) bs
  ExprAbs _ arg body -> do
    lvl <- currentLevel
    make $ Abs (NonEmptyArray.singleton (Tuple (Just arg) lvl)) (intro [ arg ] lvl (toBackendExpr body))
  ExprApp _ a b
    | ExprVar (Ann { meta: Just IsNewtype }) _ <- a ->
        toBackendExpr b
    | otherwise ->
        make $ App (toBackendExpr a) (NonEmptyArray.singleton (toBackendExpr b))
  ExprLet _ binds body ->
    foldr go (toBackendExpr body) binds
    where
    go bind' next = case bind' of
      Rec bindings -> do
        lvl <- currentLevel
        let idents = (\(Binding _ ident _) -> ident) <$> bindings
        join $ (\x y -> buildM (LetRec lvl x y))
          <$> intro idents lvl (traverse toBackendBinding bindings)
          <*> intro idents lvl next
      NonRec (Binding _ ident expr) ->
        makeLet (Just ident) (toBackendExpr expr) \_ -> next
  ExprCase _ exprs alts -> do
    foldr
      ( \expr next idents ->
          makeLet Nothing (toBackendExpr expr) \tmp ->
            next (Array.snoc idents tmp)
      )
      ( \idents -> do
          env <- currentInlineEnv
          foldr (lift2 (mergeBranches env)) patternFail $ goAlt idents <$> alts
      )
      exprs
      []
  where
  mergeBranches :: BackendInlineEnv -> BackendExpr -> BackendExpr -> BackendExpr
  mergeBranches env = case _, _ of
    BackendExpr e1 a1 (Branch bs1), BackendExpr _ a2 (Branch bs2)
      | Just (Pair (BackendExpr _ _ (Lit (LitBoolean true))) _) <- Array.last bs1 ->
          BackendExpr e1 a1 (Branch bs1)
      | otherwise ->
          BackendExpr e1 (a1 <> a2) (Branch (bs1 <> bs2))
    BackendExpr _ _ (Branch bs1), nonBranch ->
      build env (Branch (Array.snoc bs1 (Pair (build env (Lit (LitBoolean true))) nonBranch)))
    nonBranch, _ ->
      nonBranch

  goAlt :: Array Level -> CaseAlternative Ann -> BackendM BackendExpr
  goAlt idents (CaseAlternative binders branch) =
    goBinders
      ( \renames -> foldr
          ( \(Tuple a b) next ->
              makeLet (Just a) (make (Local Nothing b)) \_ -> next
          )
          (goCaseGuard branch)
          renames
      )
      List.Nil
      (List.fromFoldable idents)
      (foldr (\b s -> PatBinder b (PatPop s)) PatNil binders)

  goCaseGuard :: CaseGuard Ann -> BackendM BackendExpr
  goCaseGuard = case _ of
    Unconditional expr ->
      toBackendExpr expr
    Guarded gs ->
      buildM <<< Branch =<< traverse (\(Guard a b) -> Pair <$> toBackendExpr a <*> toBackendExpr b) gs

  goBinders
    :: (List (Tuple Ident Level) -> BackendM BackendExpr)
    -> List (Tuple Ident Level)
    -> List Level
    -> PatternStk
    -> BackendM BackendExpr
  goBinders k store stk = case _ of
    PatBinder binder next ->
      case binder, stk of
        BinderNull _, _ ->
          goBinders k store stk next
        BinderVar _ a, List.Cons id _ ->
          goBinders k (List.Cons (Tuple a id) store) stk next
        BinderNamed _ a b, List.Cons id _ ->
          goBinders k (List.Cons (Tuple a id) store) stk (PatBinder b next)
        BinderLit _ lit, List.Cons id _ -> do
          case lit of
            LitInt n ->
              makeGuard id (GuardInt n) $ goBinders k store stk next
            LitNumber n ->
              makeGuard id (GuardNumber n) $ goBinders k store stk next
            LitString n ->
              makeGuard id (GuardString n) $ goBinders k store stk next
            LitChar n ->
              makeGuard id (GuardChar n) $ goBinders k store stk next
            LitBoolean n ->
              makeGuard id (GuardBoolean n) $ goBinders k store stk next
            LitArray bs ->
              makeGuard id (GuardArrayLength (Array.length bs)) $ goBinders k store stk $ foldrWithIndex
                ( \ix b s ->
                    PatPush (GetIndex ix) $ PatBinder b $ PatPop s
                )
                next
                bs
            LitRecord ps ->
              goBinders k store stk $ foldr
                ( \(Prop ix b) s ->
                    PatPush (GetProp ix) $ PatBinder b $ PatPop s
                )
                next
                ps
        BinderConstructor (Ann { meta: Just IsNewtype }) _ _ [ b ], _ ->
          goBinders k store stk (PatBinder b next)
        BinderConstructor _ _ tag bs, List.Cons id _ ->
          makeGuard id (GuardTag tag) $ goBinders k store stk $ foldrWithIndex
            ( \ix b s ->
                PatPush (GetOffset ix) $ PatBinder b $ PatPop s
            )
            next
            bs
        _, _ ->
          unsafeCrashWith "impossible: goBinders (binder)"
    PatPush accessor next ->
      case stk of
        List.Cons id _ ->
          makeLet Nothing (make (Accessor (make (Local Nothing id)) accessor)) \tmp ->
            goBinders k store (List.Cons tmp stk) next
        _ ->
          unsafeCrashWith "impossible: goBinders (push)"
    PatPop next ->
      case stk of
        List.Cons _ stk' ->
          goBinders k store stk' next
        List.Nil ->
          unsafeCrashWith "impossible: goBinders (pop)"
    PatNil ->
      k store

  patternFail :: BackendM BackendExpr
  patternFail =
    make (Branch [ Pair (make (Lit (LitBoolean true))) (make (Fail "Failed pattern match")) ])

  makeLet :: Maybe Ident -> BackendM BackendExpr -> (Level -> BackendM BackendExpr) -> BackendM BackendExpr
  makeLet id a k = do
    lvl <- currentLevel
    case id of
      Nothing ->
        make $ Let id lvl a (levelUp (k lvl))
      Just ident ->
        make $ Let id lvl a (intro [ ident ] lvl (k lvl))

  makeGuard :: Level -> BackendGuard -> BackendM BackendExpr -> BackendM BackendExpr
  makeGuard lvl g inner =
    make $ Branch [ Pair (make (Test (make (Local Nothing lvl)) g)) inner ]

  make :: BackendExprView (BackendM BackendExpr) -> BackendM BackendExpr
  make a = buildM =<< sequence a

toBackendBinding :: Binding Ann -> BackendM (Tuple Ident BackendExpr)
toBackendBinding (Binding _ ident expr) = Tuple ident <$> toBackendExpr expr

luaCodegenModule :: forall a. BackendModule -> Dodo.Doc a
luaCodegenModule (BackendModule mod) =
  Dodo.lines
    [ Dodo.lines $ uncurry luaImport <$> mod.imports
    , Dodo.lines $ luaCodegenBindingGroup <<< withInlining =<< mod.bindings
    , Dodo.words
        [ Dodo.text "return"
        , luaRecord (map (uncurry luaExport) mod.exports)
        ]
    ]
  where
  inliningEnv =
    { currentModule: mod.name
    , currentLevel: Level 0
    , vars: Map.empty
    }

  withInlining :: BackendBindingGroup (Tuple BackendExpr (Maybe (Expr Ann))) -> BackendBindingGroup BackendExpr
  withInlining r = do
    -- TODO: recursive group
    let
      { value: newBindings } = mapAccumL
        ( \env (Tuple ident (Tuple expr impl)) ->
            { accum: varImpl ident (maybe ForeignImpl (CoreFnImpl (analysisOf expr)) impl) env
            , value: Tuple ident (review (withVars env.vars expr))
            }
        )
        inliningEnv
        r.bindings
    r { bindings = newBindings }

  varImpl :: Ident -> VarImpl -> BackendInlineEnv -> BackendInlineEnv
  varImpl ident impl env = env { vars = Map.insert (Qualified Nothing ident) impl env.vars }

luaCodegenExpr :: forall a. BackendExpr -> Dodo.Doc a
luaCodegenExpr a = luaCodegenExprView (view a)

luaCodegenExprView :: forall a. BackendExprView BackendExpr -> Dodo.Doc a
luaCodegenExprView = case _ of
  Var (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) ->
    luaUndefined
  Var var ->
    luaCodegenQualified luaCodegenIdent var
  Local ident lvl ->
    luaCodegenLocal ident lvl
  Lit lit ->
    luaCodegenLit lit
  App a bs ->
    luaCurriedApp (luaCodegenExpr a) (luaCodegenExpr <$> bs)
  Abs idents body
    | [ Tuple (Just (Ident "$__unused")) _ ] <- NonEmptyArray.toArray idents ->
        luaFn [] (luaCodegenBlockStatements (view body))
    | otherwise ->
        luaCurriedFn idents (luaCodegenBlockStatements (view body))
  Accessor a prop ->
    luaCodegenAccessor (luaCodegenExpr a) prop
  Update a props ->
    luaUpdate (luaCodegenExpr a) (map luaCodegenExpr <$> props)
  CtorDef (Ident tag) fields -> do
    let fieldsWithLevel = mapWithIndex (\ix ident -> Tuple (Just ident) (Level ix)) fields
    luaCurriedFn fieldsWithLevel (luaCtor tag (uncurry luaCodegenLocal <$> fieldsWithLevel))
  Test a b ->
    luaCodegenTest (luaCodegenExpr a) b
  Fail str ->
    luaError str
  expr@(Branch _) ->
    luaCodegenBlock expr
  expr@(LetRec _ _ _) ->
    luaCodegenBlock expr
  expr@(Let _ _ _ _) ->
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
    LetRec lvl bindings body -> do
      let lines = luaCodegenBindingGroup { recursive: true, bindings: lmap (flip luaLocalIdent lvl <<< Just) <$> bindings }
      go (acc <> lines) (view body)
    Let ident lvl expr body -> do
      let lines = luaCodegenBindingGroup { recursive: false, bindings: [ Tuple (luaLocalIdent ident lvl) expr ] }
      go (acc <> lines) (view body)
    Branch bs ->
      Array.snoc acc (luaCodegenBlockBranches bs)
    expr@(Fail _) ->
      Array.snoc acc (luaCodegenExprView expr)
    expr ->
      Array.snoc acc (luaReturn (luaCodegenExprView expr))

luaCodegenBindingGroup :: forall a. BackendBindingGroup BackendExpr -> Array (Dodo.Doc a)
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

luaLocalIdent :: Maybe Ident -> Level -> Ident
luaLocalIdent mb (Level lvl) = case mb of
  Just (Ident a) ->
    Ident (a <> "." <> show lvl)
  Nothing ->
    Ident ("." <> show lvl)

luaCodegenLocal :: forall a. Maybe Ident -> Level -> Dodo.Doc a
luaCodegenLocal a b = luaCodegenIdent (luaLocalIdent a b)

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

luaFn :: forall a. Array (Tuple (Maybe Ident) Level) -> Dodo.Doc a -> Dodo.Doc a
luaFn args stmts =
  Dodo.lines
    [ Dodo.text "function" <> Dodo.Common.jsParens
        (Dodo.foldWithSeparator Dodo.Common.trailingComma (uncurry luaCodegenLocal <$> args))
    , Dodo.indent $ stmts
    , Dodo.text "end"
    ]

luaReturn :: forall a. Dodo.Doc a -> Dodo.Doc a
luaReturn doc = Dodo.flexGroup $ fold
  [ Dodo.text "return"
  , Dodo.space
  , Dodo.indent doc
  ]

luaCurriedFn :: forall f a. Foldable f => f (Tuple (Maybe Ident) Level) -> Dodo.Doc a -> Dodo.Doc a
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
luaExport (Ident ident) ref = Prop (luaEscapeIdent ident) (luaCodegenQualified luaCodegenIdent ref)

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