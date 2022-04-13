module Backend where

import Prelude

import Control.Alternative (guard)
import Control.Monad.RWS (RWST, ask, evalRWST, modify_, state)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (class Foldable, fold, foldl, foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function (on)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..), fst, uncurry)
import Dodo as Dodo
import Dodo.Common (jsCurlies, jsParens, jsSquares, trailingComma)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Meta(..), Module(..), ModuleName(..), Prop(..), Qualified(..), ReExport(..))

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

type BackendM = RWST BackendEnv Unit BackendState Identity

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
  bindings <- backendBindingGroups mod.decls
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
  | Accessor BackendExpr String
  | Update BackendExpr (Array (Prop BackendExpr))
  | CtorDef Ident (Array Ident)
  | Let BackendBindingGroup BackendExpr
  | Case (Array BackendExpr) BackendPattern
  | OptInlineComp BackendExpr (NonEmptyArray BackendExpr)

type PatternRenaming = List Ident

data BackendPattern
  = Branch BackendPattern BackendPattern
  | Command BackendPatternCmd BackendPattern
  | PatternGuard BackendPatternGuard BackendPattern
  | Match BackendExpr
  | Rename PatternRenaming BackendPattern
  | Fail

data BackendPatternGuard
  = GuardNumber Number
  | GuardInt Int
  | GuardString String
  | GuardBoolean Boolean
  | GuardChar Char
  | GuardTag (Qualified Ident)
  | GuardArrayLength Int
  | GuardExpr BackendExpr

data BackendPatternCmd
  = PushProp String
  | PushIndex Int
  | PushOffset Int
  | PushCase Int
  | Store
  | Pop

backendExpr :: Expr Ann -> BackendM BackendExpr
backendExpr = go
  where
  go :: Expr Ann -> BackendM BackendExpr
  go expr = rewrite <$> goExpr expr

  rewrite :: BackendExpr -> BackendExpr
  rewrite expr = optimizeExpr (simplifyExpr expr)

  simplifyExpr :: BackendExpr -> BackendExpr
  simplifyExpr = case _ of
    App (App a bs) cs ->
      App a (bs <> cs)
    Abs as (Abs bs expr) ->
      Abs (as <> bs) expr
    Accessor (Lit (LitRecord props)) prop
      | Just expr <- Array.findMap (\(Prop prop' value) -> guard (prop == prop') $> value) props ->
          expr
    Let { bindings: [] } expr ->
      expr
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

  goExpr :: Expr Ann -> BackendM BackendExpr
  goExpr = case _ of
    ExprVar _ qi@(Qualified mn ident) -> do
      { currentModule } <- ask
      if mn == Just currentModule then
        pure (Var (Qualified Nothing ident))
      else
        pure (Var qi)
    ExprLit _ lit ->
      Lit <$> traverse go lit
    ExprConstructor _ _ name fields ->
      pure (CtorDef name fields)
    ExprAccessor _ a field ->
      Accessor <$> go a <*> pure field
    ExprUpdate _ a bs ->
      Update <$> go a <*> traverse (traverse go) bs
    ExprAbs _ arg body ->
      Abs (NonEmptyArray.singleton arg) <$> go body
    ExprApp _ a b ->
      App <$> go a <*> (NonEmptyArray.singleton <$> go b)
    ExprCase _ exprs alts ->
      Case <$> traverse go exprs <*> goAlts alts
    ExprLet _ binds body ->
      flip (foldr Let) <$> backendBindingGroups binds <*> goExpr body

  goAlts :: Array (CaseAlternative Ann) -> BackendM BackendPattern
  goAlts alts = do
    let
      go' alt next =
        Branch <$> goAlt alt <*> next
    foldr go' (pure Fail) alts

  goAlt :: CaseAlternative Ann -> BackendM BackendPattern
  goAlt (CaseAlternative binders guard) = do
    result <- case guard of
      Unconditional expr ->
        Rename (renamingsFromBinders binders) <<< Match <$> go expr
      Guarded gs -> do
        let
          go' (Guard a b) next = do
            a' <- go a
            b' <- go b
            Branch (PatternGuard (GuardExpr a') (Match b')) <$> next
        Rename (renamingsFromBinders binders) <$> foldr go' (pure Fail) gs
    let
      go' ix b next' =
        Command (PushCase ix) <$> (goBinder b <<< Command Pop =<< next')
    foldrWithIndex go' (pure result) binders

  goBinder :: Binder Ann -> BackendPattern -> BackendM BackendPattern
  goBinder binder next = case binder of
    BinderNull _ ->
      pure next
    BinderVar _ _ ->
      pure $ Command Store next
    BinderNamed _ _ b ->
      Command Store <$> goBinder b next
    BinderLit _ lit ->
      case lit of
        LitInt n ->
          pure $ Branch (PatternGuard (GuardInt n) next) Fail
        LitNumber n ->
          pure $ Branch (PatternGuard (GuardNumber n) next) Fail
        LitString str ->
          pure $ Branch (PatternGuard (GuardString str) next) Fail
        LitChar ch ->
          pure $ Branch (PatternGuard (GuardChar ch) next) Fail
        LitBoolean bool ->
          pure $ Branch (PatternGuard (GuardBoolean bool) next) Fail
        LitArray bs -> do
          let
            go' ix b next' =
              Command (PushIndex ix) <$> (goBinder b <<< Command Pop =<< next')
          bs' <- foldrWithIndex go' (pure next) bs
          pure $ Branch (PatternGuard (GuardArrayLength (Array.length bs)) bs') Fail
        LitRecord bs -> do
          let
            go' (Prop p b) next' =
              Command (PushProp p) <$> (goBinder b <<< Command Pop =<< next')
          foldr go' (pure next) bs
    BinderConstructor (Ann { meta: Just IsNewtype }) _ _ [ b ] ->
      goBinder b next
    BinderConstructor _ _ ident bs -> do
      let
        go' ix b next' =
          Command (PushOffset ix) <$> (goBinder b <<< Command Pop =<< next')
      bs' <- foldrWithIndex go' (pure next) bs
      pure $ Branch (PatternGuard (GuardTag ident) bs') Fail

backendBindingGroups :: Array (Bind Ann) -> BackendM (Array BackendBindingGroup)
backendBindingGroups binds = do
  binds' <- traverse backendBindingGroup binds
  pure $ (\as -> { recursive: (NonEmptyArray.head as).recursive, bindings: _.bindings =<< NonEmptyArray.toArray as }) <$>
    Array.groupBy ((&&) `on` (not <<< _.recursive)) binds'

backendBindingGroup :: Bind Ann -> BackendM BackendBindingGroup
backendBindingGroup = case _ of
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

renamingsFromBinders :: forall a. Array (Binder a) -> PatternRenaming
renamingsFromBinders = foldl go List.Nil
  where
  go :: PatternRenaming -> Binder a -> PatternRenaming
  go acc = case _ of
    BinderVar _ ident ->
      List.Cons ident acc
    BinderNamed _ ident b -> do
      go (List.Cons ident acc) b
    BinderLit _ (LitArray bs) ->
      foldl go acc bs
    BinderLit _ (LitRecord bs) ->
      foldl (\acc' (Prop _ b) -> go acc' b) acc bs
    BinderConstructor _ _ _ bs ->
      foldl go acc bs
    _ ->
      acc

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
        luaFn [] [] <$> codegenExpr body
    | otherwise ->
        luaCurriedFn idents <$> codegenExpr body
  Accessor a prop ->
    flip luaAccessor prop <$> codegenExpr a
  Update a props -> do
    tmp1 <- tmpIdent
    tmp2 <- tmpIdent
    luaCopyTo tmp1 tmp2
      <$> codegenExpr a
      <*> map (map (luaAssignProp tmp2)) (traverse (traverse codegenExpr) props)
  CtorDef (Ident tag) fields ->
    pure $ luaCurriedFn fields (luaCtor tag (codegenIdent <$> fields))
  Let bindingGroup body ->
    luaBlock <$> codegenBindingGroup bindingGroup <*> codegenExpr body
  Case exprs pattern -> do
    tmps <- sequence $ Array.replicate (Array.length exprs) tmpIdent
    exprs' <- traverse codegenExpr exprs
    pattern' <- codegenPattern tmps pattern
    pure $ luaBlock (Array.snoc (Array.zipWith luaAssign tmps exprs') pattern') mempty
  OptInlineComp hd tl -> do
    bindings <- for (NonEmptyArray.toArray (NonEmptyArray.cons hd tl)) \expr ->
      Tuple <$> tmpIdent <*> codegenExpr expr
    arg <- tmpIdent
    pure $ luaBlock (uncurry luaBinding <$> bindings)
      ( luaFn [ arg ] []
          ( foldr (\(Tuple fn _) a -> luaApp (codegenIdent fn) [ a ])
              (codegenIdent arg)
              bindings
          )
      )

codegenPattern :: forall a. Array Ident -> BackendPattern -> BackendM (Dodo.Doc a)
codegenPattern caseIdents =
  ( \p -> do
      failLbl <- tmpIdent
      branches <- go List.Nil (List.singleton failLbl) List.Nil p
      if isTotal p then
        pure branches
      else
        pure $ Dodo.lines
          [ branches
          , luaLabel failLbl
          , luaApp (Dodo.text "error") [ luaString "Failed pattern match." ]
          ]
  )
  where
  isTotal = case _ of
    Branch a Fail -> isTotal' a
    Branch _ a -> isTotal a
    a -> isTotal' a

  isTotal' = case _ of
    PatternGuard _ a -> isTotal' a
    Command _ a -> isTotal' a
    Rename _ a -> isTotal' a
    Match _ -> true
    _ -> false

  go :: List Ident -> List Ident -> List Ident -> BackendPattern -> BackendM (Dodo.Doc a)
  go stk alts store = case _ of
    Branch p1 Fail ->
      go stk alts store p1
    Branch p1 p2 -> do
      altLbl <- tmpIdent
      branch1 <- go stk (List.Cons altLbl alts) store p1
      branch2 <- go stk alts store p2
      pure $ Dodo.lines
        [ branch1
        , luaLabel altLbl
        , branch2
        ]
    Command cmd next -> do
      case cmd, stk of
        PushProp prop, List.Cons val _ -> do
          tmp <- tmpIdent
          next' <- go (List.Cons tmp stk) alts store next
          pure $ Dodo.lines [ luaBinding tmp (luaAccessor (codegenIdent val) prop), next' ]
        PushIndex ix, List.Cons val _ -> do
          tmp <- tmpIdent
          next' <- go (List.Cons tmp stk) alts store next
          pure $ Dodo.lines [ luaBinding tmp (luaIndex (codegenIdent val) ix), next' ]
        PushOffset off, List.Cons val _ -> do
          tmp <- tmpIdent
          next' <- go (List.Cons tmp stk) alts store next
          pure $ Dodo.lines [ luaBinding tmp (luaOffset (codegenIdent val) off), next' ]
        PushCase cs, _ ->
          go (List.Cons (unsafePartial Array.unsafeIndex caseIdents cs) stk) alts store next
        Store, List.Cons val _ ->
          go stk alts (List.Cons val store) next
        Pop, List.Cons _ stk' ->
          go stk' alts store next
        _, List.Nil ->
          unsafeCrashWith "codegenPattern: impossible Command"
    PatternGuard grd next -> do
      next' <- go stk alts store next
      case alts of
        List.Cons altLbl _ -> do
          cond <- case grd, stk of
            GuardNumber n, List.Cons val _ ->
              pure $ Dodo.words [ codegenIdent val, Dodo.text "==", luaNumber n ]
            GuardInt n, List.Cons val _ ->
              pure $ Dodo.words [ codegenIdent val, Dodo.text "==", luaInt n ]
            GuardString str, List.Cons val _ ->
              pure $ Dodo.words [ codegenIdent val, Dodo.text "==", luaString str ]
            GuardBoolean bool, List.Cons val _ ->
              pure $ Dodo.words [ codegenIdent val, Dodo.text "==", luaBoolean bool ]
            GuardChar ch, List.Cons val _ ->
              pure $ Dodo.words [ codegenIdent val, Dodo.text "==", luaChar ch ]
            GuardTag (Qualified _ (Ident tag)), List.Cons val _ -> do
              pure $ Dodo.words [ luaIndex (codegenIdent val) 0, Dodo.text "==", luaString tag ]
            GuardArrayLength len, List.Cons val _ -> do
              pure $ Dodo.words [ codegenIdent val <> Dodo.text ".n", Dodo.text "==", luaInt len ]
            GuardExpr expr, _ ->
              codegenExpr expr
            _, List.Nil ->
              unsafeCrashWith "codegenPattern: impossible PatternGuard empty stk"
          pure $ luaIfElse [ Tuple cond next' ] (luaGoto altLbl)
        _ ->
          unsafeCrashWith "codegenPattern: impossible PatternGuard empty alts"
    Match expr ->
      luaReturn <$> codegenExpr expr
    Rename rename next -> do
      let names = List.zipWith (\a b -> luaBinding a (codegenIdent b)) rename store
      Dodo.appendBreak (Dodo.lines names) <$> go stk alts List.Nil next
    Fail ->
      case alts of
        List.Cons altLbl _ ->
          pure $ luaGoto altLbl
        _ ->
          unsafeCrashWith "Bad pattern tree"

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

luaBlock :: forall a. Array (Dodo.Doc a) -> Dodo.Doc a -> Dodo.Doc a
luaBlock stmts expr = jsParens (luaFn mempty stmts expr) <> Dodo.text "()"

luaFn :: forall a. Array Ident -> Array (Dodo.Doc a) -> Dodo.Doc a -> Dodo.Doc a
luaFn args stmts expr =
  Dodo.lines
    [ Dodo.text "function" <> jsParens (Dodo.foldWithSeparator trailingComma (codegenIdent <$> args))
    , Dodo.indent $ Dodo.lines
        [ Dodo.lines stmts
        , Monoid.guard (not (Dodo.isEmpty expr)) $ luaReturn expr
        ]
    , Dodo.text "end"
    ]

luaReturn :: forall a. Dodo.Doc a -> Dodo.Doc a
luaReturn doc = Dodo.flexGroup $ fold
  [ Dodo.text "return"
  , Dodo.space
  , Dodo.indent doc
  ]

luaCurriedFn :: forall f a. Foldable f => f Ident -> Dodo.Doc a -> Dodo.Doc a
luaCurriedFn = flip (foldr (flip luaFn [] <<< pure))

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
  , Dodo.text "else"
  , Dodo.indent default
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