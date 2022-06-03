module PureScript.Backend.Codegen.Lua where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (class Foldable, fold, foldl, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Convert (BackendBindingGroup, BackendModule)
import PureScript.Backend.Semantics (NeutralExpr(..))
import PureScript.Backend.Syntax (BackendAccessor(..), BackendGuard(..), BackendSyntax(..), Level(..), Pair(..))
import PureScript.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), Qualified(..))

type CodegenEnv =
  { currentModule :: ModuleName
  , bound :: Map Ident Int
  , names :: Map (Tuple Ident Level) Ident
  }

freshName :: Maybe Ident -> Level -> CodegenEnv -> Tuple Ident CodegenEnv
freshName ident lvl env = case ident of
  Nothing ->
    Tuple (Ident ("." <> show (unwrap lvl))) env
  Just id ->
    case Map.lookup id env.bound of
      Nothing ->
        Tuple id $ env
          { bound = Map.insert id 1 env.bound
          , names = Map.insert (Tuple id lvl) id env.names
          }
      Just n -> do
        let fresh = Ident (unwrap id <> "." <> show n)
        Tuple fresh $ env
          { bound = Map.insert id (n + 1) env.bound
          , names = Map.insert (Tuple id lvl) fresh env.names
          }

rename :: Maybe Ident -> Level -> CodegenEnv -> Ident
rename ident lvl env =
  case ident >>= \id -> Map.lookup (Tuple id lvl) env.names of
    Nothing ->
      luaLocalIdent ident lvl
    Just id ->
      id

luaCodegenModule :: forall a. BackendModule -> Dodo.Doc a
luaCodegenModule mod@{ name: ModuleName this } = do
  let
    foreignModuleName =
      ModuleName (this <> "$foreign")

    foreignBindings = map
      (\ident -> Tuple ident $ NeutralExpr $ Var $ Qualified (Just foreignModuleName) ident)
      mod.foreign

    moduleBindings =
      Array.cons { recursive: false, bindings: foreignBindings } mod.bindings

    codegenEnv =
      { currentModule: mod.name
      , bound: Map.empty
      , names: Map.empty
      }

  Dodo.lines
    [ Dodo.lines $ (\mn -> luaImport mn (luaModulePath mn)) <$> mod.imports
    , Monoid.guard (not (Array.null foreignBindings)) $ luaImport foreignModuleName (luaForeignModulePath mod.name)
    , Dodo.lines $ luaCodegenBindingGroup codegenEnv =<< moduleBindings
    , Dodo.words
        [ Dodo.text "return"
        , luaRecord (map (uncurry luaExport) mod.exports)
        ]
    ]

luaCodegenExpr :: forall a. CodegenEnv -> NeutralExpr -> Dodo.Doc a
luaCodegenExpr env (NeutralExpr expr) = luaCodegenExprSyntax env expr

luaCodegenExprSyntax :: forall a. CodegenEnv -> BackendSyntax NeutralExpr -> Dodo.Doc a
luaCodegenExprSyntax env = case _ of
  Var (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) ->
    luaUndefined
  Var (Qualified (Just mn) ident) | mn == env.currentModule ->
    luaCodegenIdent ident
  Var var ->
    luaCodegenQualified luaCodegenIdent var
  Local ident lvl ->
    luaCodegenIdent (rename ident lvl env)
  Lit lit ->
    luaCodegenLit env lit
  App a bs ->
    luaCurriedApp (luaCodegenExpr env a) (luaCodegenExpr env <$> bs)
  Abs idents (NeutralExpr body)
    | [ Tuple (Just (Ident "$__unused")) _ ] <- NonEmptyArray.toArray idents ->
        luaFn [] (luaCodegenBlockStatements env body)
    | otherwise -> do
        let
          result = mapAccumL
            ( \env' (Tuple ident lvl) -> do
                let Tuple newIdent env'' = freshName ident lvl env'
                { accum: env''
                , value: newIdent
                }
            )
            env
            idents
        luaCurriedFn result.value (luaCodegenBlockStatements result.accum body)
  Accessor a prop ->
    luaCodegenAccessor (luaCodegenExpr env a) prop
  Update a props ->
    luaUpdate (luaCodegenExpr env a) (map (luaCodegenExpr env) <$> props)
  CtorDef (Ident tag) fields -> do
    luaCurriedFn fields (luaCtor tag (luaCodegenIdent <$> fields))
  CtorSaturated _ (Ident tag) fields ->
    luaCtor tag (luaCodegenExpr env <<< snd <$> fields)
  Test a b ->
    luaCodegenTest (luaCodegenExpr env a) b
  Fail str ->
    luaError str
  expr@(Branch _ _) ->
    luaCodegenBlock env expr
  expr@(LetRec _ _ _) ->
    luaCodegenBlock env expr
  expr@(Let _ _ _ _) ->
    luaCodegenBlock env expr
  expr@(EffectBind _ _ _ _) ->
    luaCodegenEffectBlock env expr
  expr@(EffectPure _) ->
    luaCodegenEffectBlock env expr

luaCodegenLit :: forall a. CodegenEnv -> Literal NeutralExpr -> Dodo.Doc a
luaCodegenLit env = case _ of
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
    luaArray (luaCodegenExpr env <$> as)
  LitRecord props ->
    luaRecord (map (luaCodegenExpr env) <$> props)

luaCodegenBlock :: forall a. CodegenEnv -> BackendSyntax NeutralExpr -> Dodo.Doc a
luaCodegenBlock env a = luaBlock (luaCodegenBlockStatements env a)

luaCodegenEffectBlock :: forall a. CodegenEnv -> BackendSyntax NeutralExpr -> Dodo.Doc a
luaCodegenEffectBlock env a = luaEffectBlock (luaCodegenEffectBlockStatements env a)

luaCodegenBlockBranches :: forall a. CodegenEnv -> Array (Pair NeutralExpr) -> Maybe NeutralExpr -> Dodo.Doc a
luaCodegenBlockBranches env bs def = luaBranches (go <$> bs) (luaCodegenBlockStatements env <<< unwrap <$> def)
  where
  go :: Pair NeutralExpr -> Tuple (Dodo.Doc a) (Dodo.Doc a)
  go (Pair a (NeutralExpr b)) = case b of
    Branch next nextDef ->
      Tuple (luaCodegenExpr env a) (luaCodegenBlockBranches env next nextDef)
    b' ->
      Tuple (luaCodegenExpr env a) (luaCodegenBlockStatements env b')

luaCodegenBlockStatements :: forall a. CodegenEnv -> BackendSyntax NeutralExpr -> Dodo.Doc a
luaCodegenBlockStatements = (\env -> luaStatements <<< go env [])
  where
  go :: CodegenEnv -> Array (Dodo.Doc a) -> BackendSyntax NeutralExpr -> Array (Dodo.Doc a)
  go env acc = case _ of
    LetRec lvl bindings (NeutralExpr body) -> do
      let
        result = mapAccumL
          ( \env' (Tuple ident binding) -> do
              let Tuple newIdent env'' = freshName (Just ident) lvl env'
              { accum: env''
              , value: Tuple newIdent binding
              }
          )
          env
          bindings
      let lines = luaCodegenBindingGroup result.accum { recursive: true, bindings: result.value }
      go result.accum (acc <> lines) body
    Let ident lvl expr (NeutralExpr body) -> do
      let Tuple newIdent env' = freshName ident lvl env
      let lines = luaCodegenBindingGroup env { recursive: false, bindings: [ Tuple newIdent expr ] }
      go env' (acc <> lines) body
    Branch bs def ->
      Array.snoc acc (luaCodegenBlockBranches env bs def)
    expr@(Fail _) ->
      Array.snoc acc (luaCodegenExprSyntax env expr)
    expr ->
      Array.snoc acc (luaReturn (luaCodegenExprSyntax env expr))

luaCodegenEffectBlockStatements :: forall a. CodegenEnv -> BackendSyntax NeutralExpr -> Dodo.Doc a
luaCodegenEffectBlockStatements = (\env -> luaStatements <<< go env [])
  where
  go :: CodegenEnv -> Array (Dodo.Doc a) -> BackendSyntax NeutralExpr -> Array (Dodo.Doc a)
  go env acc = case _ of
    LetRec lvl bindings (NeutralExpr body) -> do
      let
        result = mapAccumL
          ( \env' (Tuple ident binding) -> do
              let Tuple newIdent env'' = freshName (Just ident) lvl env'
              { accum: env''
              , value: Tuple newIdent binding
              }
          )
          env
          bindings
      let lines = luaCodegenBindingGroup result.accum { recursive: true, bindings: result.value }
      go result.accum (acc <> lines) body
    Let ident lvl expr (NeutralExpr body) -> do
      let Tuple newIdent env' = freshName ident lvl env
      let lines = luaCodegenBindingGroup env { recursive: false, bindings: [ Tuple newIdent expr ] }
      go env' (acc <> lines) body
    EffectBind ident lvl eff (NeutralExpr body) -> do
      let Tuple newIdent env' = freshName ident lvl env
      let line = luaBinding newIdent (luaApp (luaCodegenExpr env eff) [])
      go env' (Array.snoc acc line) body
    EffectPure a ->
      Array.snoc acc (luaReturn (luaCodegenExpr env a))
    Branch bs def ->
      Array.snoc acc (luaCodegenEffectBlockBranches env bs def)
    expr@(Fail _) ->
      Array.snoc acc (luaCodegenExprSyntax env expr)
    expr ->
      Array.snoc acc (luaReturn (luaCodegenExprSyntax env expr))

luaCodegenEffectBlockBranches :: forall a. CodegenEnv -> Array (Pair NeutralExpr) -> Maybe NeutralExpr -> Dodo.Doc a
luaCodegenEffectBlockBranches env bs def = luaBranches (go <$> bs) (luaCodegenEffectBlockStatements env <<< unwrap <$> def)
  where
  go :: Pair NeutralExpr -> Tuple (Dodo.Doc a) (Dodo.Doc a)
  go (Pair a (NeutralExpr b)) = case b of
    Branch next nextDef ->
      Tuple (luaCodegenExpr env a) (luaCodegenEffectBlockBranches env next nextDef)
    b' ->
      Tuple (luaCodegenExpr env a) (luaCodegenEffectBlockStatements env b')

luaCodegenBindingGroup :: forall a. CodegenEnv -> BackendBindingGroup NeutralExpr -> Array (Dodo.Doc a)
luaCodegenBindingGroup env { recursive, bindings }
  | recursive = do
      let fwdRefs = luaFwdRef <<< fst <$> bindings
      fwdRefs <> map (\(Tuple ident b) -> luaAssign ident (luaCodegenExpr env b)) bindings
  | otherwise =
      map (\(Tuple ident b) -> luaBinding ident (luaCodegenExpr env b)) bindings

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

luaEffectBlock :: forall a. Dodo.Doc a -> Dodo.Doc a
luaEffectBlock stmts = Dodo.Common.jsParens (luaFn mempty stmts)

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

luaBranches :: forall a. Array (Tuple (Dodo.Doc a) (Dodo.Doc a)) -> Maybe (Dodo.Doc a) -> Dodo.Doc a
luaBranches branches def =
  Dodo.lines
    [ Dodo.lines $ map
        ( \(Tuple doc1 doc2) ->
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
        )
        branches
    , fold def
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