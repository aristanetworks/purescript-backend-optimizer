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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, uncurry)
import Dodo as Dodo
import Dodo.Common (jsCurlies, jsParens, jsSquares, trailingComma)
import PureScript.CoreFn (Ann, Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Module(..), ModuleName(..), Prop(..), Qualified(..), ReExport(..))

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
tmpIdent = state \st -> Tuple (Ident ("_t" <> show st.fresh)) (st { fresh = st.fresh + 1 })

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
        [ map (\(Import _ mn) -> Tuple mn (luaModulePath mn)) mod.imports
        , Monoid.guard (not (Array.null mod.foreign))
            [ Tuple foreignModuleName (this <> ".foreign.lua") ]
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
  go expr = rewriteExpr <$> goExpr expr

  rewriteExpr :: BackendExpr -> BackendExpr
  rewriteExpr = case _ of
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

  goExpr :: Expr Ann -> BackendM BackendExpr
  goExpr = case _ of
    ExprVar _ qi ->
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
  Var var ->
    pure $ codegenQualified codegenIdent var
  Lit lit ->
    codegenLit lit
  App a bs ->
    luaCurriedApp <$> codegenExpr a <*> traverse codegenExpr bs
  Abs idents body ->
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
  Case _ _ ->
    pure mempty

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
    pure $ Dodo.text (show n)
  LitNumber n ->
    pure $ Dodo.text (show n)
  LitString str ->
    pure $ Dodo.text (show str)
  LitChar ch ->
    pure $ Dodo.text (show ch)
  LitBoolean bool ->
    pure $ Dodo.text (show bool)
  LitArray as ->
    luaArray <$> traverse codegenExpr as
  LitRecord props ->
    luaRecord <$> traverse (traverse codegenExpr) props

codegenIdent :: forall a. Ident -> Dodo.Doc a
codegenIdent (Ident a) = Dodo.text a

codegenQualified :: forall a b. (a -> Dodo.Doc b) -> Qualified a -> Dodo.Doc b
codegenQualified codegenInner (Qualified qual inner) = case qual of
  Nothing -> codegenInner inner
  Just mn -> codegenModuleName mn <> Dodo.text "." <> codegenInner inner

codegenModuleName :: forall a. ModuleName -> Dodo.Doc a
codegenModuleName (ModuleName mn) = Dodo.text (dotsToDollars mn)

dotsToDollars :: String -> String
dotsToDollars = String.replaceAll (Pattern ".") (Replacement "$")

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
    [ Dodo.words
        [ Dodo.text "local"
        , Dodo.text "function" <> jsParens (Dodo.foldWithSeparator trailingComma (codegenIdent <$> args))
        ]
    , Dodo.indent $ Dodo.lines
        [ Dodo.lines stmts
        , Dodo.words
            [ Dodo.text "return"
            , expr
            ]
        ]
    , Dodo.text "end"
    ]

luaCurriedFn :: forall f a. Foldable f => f Ident -> Dodo.Doc a -> Dodo.Doc a
luaCurriedFn = flip (foldr (flip luaFn [] <<< pure))

luaArray :: forall a. Array (Dodo.Doc a) -> Dodo.Doc a
luaArray = jsCurlies <<< Dodo.foldWithSeparator trailingComma

luaRecord :: forall a. Array (Prop (Dodo.Doc a)) -> Dodo.Doc a
luaRecord = jsCurlies <<< Dodo.foldWithSeparator trailingComma <<< map luaProp

luaProp :: forall a. Prop (Dodo.Doc a) -> Dodo.Doc a
luaProp (Prop prop val) = fold
  [ jsSquares (Dodo.text (show prop))
  , Dodo.space <> Dodo.text "="
  , Dodo.indent $ Dodo.flexGroup $ Dodo.spaceBreak <> val
  ]

luaCtor :: forall a. String -> Array (Dodo.Doc a) -> Dodo.Doc a
luaCtor tag vals = luaArray (Array.cons (luaString tag) vals)

luaString :: forall a. String -> Dodo.Doc a
luaString = Dodo.text <<< show

luaApp :: forall f a. Foldable f => Dodo.Doc a -> f (Dodo.Doc a) -> Dodo.Doc a
luaApp a bs = a <> jsParens (Dodo.foldWithSeparator trailingComma bs)

luaCurriedApp :: forall a. Dodo.Doc a -> NonEmptyArray (Dodo.Doc a) -> Dodo.Doc a
luaCurriedApp = foldl (\a b -> a <> jsParens b)

luaImport :: forall a. ModuleName -> String -> Dodo.Doc a
luaImport mn path = Dodo.words
  [ Dodo.text "local"
  , codegenModuleName mn
  , Dodo.text "="
  , Dodo.text "require" <> jsParens (Dodo.text (show requirePath))
  ]
  where
  requirePath =
    fromMaybe path (String.stripSuffix (Pattern ".lua") path)

luaExport :: forall a. Ident -> Qualified Ident -> Prop (Dodo.Doc a)
luaExport (Ident ident) ref = Prop ident (codegenQualified codegenIdent ref)

luaModulePath :: ModuleName -> String
luaModulePath (ModuleName mn) = mn <> ".lua"