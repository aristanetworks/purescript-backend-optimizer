module PureScript.CoreFn.Json
  ( decodeModule
  , decodeModule'
  , decodeAnn
  ) where

import Prelude

import Control.Alternative (guard, (<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Decoders (decodeArray, decodeBoolean, decodeInt, decodeJObject, decodeNumber, decodeString, decodeTuple, getField, getFieldOptional')
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (intercalate)
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Foreign.Object (Object)
import Foreign.Object as Object
import PureScript.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Comment(..), ConstructorType(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Meta(..), Module(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..), ReExport(..), SourcePos, SourceSpan)
import Safe.Coerce (coerce)

type JsonDecode = Either JsonDecodeError

decodeSourcePos :: Json -> JsonDecode SourcePos
decodeSourcePos json = do
  Tuple line column <- decodeJson json
  pure { line, column }

decodeSourceSpan :: String -> Json -> JsonDecode SourceSpan
decodeSourceSpan path json = do
  obj <- decodeJObject json
  start <- getField decodeSourcePos obj "start"
  end <- getField decodeSourcePos obj "end"
  pure { path, start, end }

decodeConstructorType :: Json -> JsonDecode ConstructorType
decodeConstructorType json = do
  str <- decodeString json
  case str of
    "ProductType" -> pure ProductType
    "SumType" -> pure SumType
    _ -> throwError $ TypeMismatch "ConstructorType"

decodeIdent :: Json -> JsonDecode Ident
decodeIdent = coerce decodeString

decodeProperName :: Json -> JsonDecode ProperName
decodeProperName = coerce decodeString

decodeModuleName :: Json -> JsonDecode ModuleName
decodeModuleName = map (ModuleName <<< intercalate ".") <<< decodeArray decodeString

decodeQualified :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Qualified a)
decodeQualified k json = do
  obj <- decodeJObject json
  moduleName <- getFieldOptional' decodeModuleName obj "moduleName"
  identifier <- getField k obj "identifier"
  pure $ Qualified moduleName identifier

decodeMeta :: Json -> JsonDecode Meta
decodeMeta json = do
  obj <- decodeJObject json
  typ <- getField decodeString obj "metaType"
  case typ of
    "IsConstructor" -> do
      ct <- getField decodeConstructorType obj "constructorType"
      is <- getField (decodeArray decodeIdent) obj "identifiers"
      pure $ IsConstructor ct is
    "IsNewtype" ->
      pure IsNewtype
    "IsTypeClassConstructor" ->
      pure IsTypeClassConstructor
    "IsForeign" ->
      pure IsForeign
    "IsWhere" ->
      pure IsWhere
    _ ->
      throwError $ TypeMismatch "Meta"

decodeAnn :: String -> Json -> JsonDecode Ann
decodeAnn path json = do
  obj <- decodeJObject json
  span <- getField (decodeSourceSpan path) obj "sourceSpan"
  meta <- getFieldOptional' decodeMeta obj "meta"
  pure $ Ann { span, meta }

decodeImport :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Import a)
decodeImport decodeAnn' json = do
  obj <- decodeJObject json
  ann <- getField decodeAnn' obj "annotation"
  mod <- getField decodeModuleName obj "moduleName"
  pure $ Import ann mod

decodeModule :: Json -> JsonDecode (Module Ann)
decodeModule = decodeModule' decodeAnn

decodeModule' :: forall a. (String -> Json -> JsonDecode a) -> Json -> JsonDecode (Module a)
decodeModule' decodeAnn' json = do
  obj <- decodeJObject json
  name <- getField decodeModuleName obj "moduleName"
  path <- getField decodeString obj "modulePath"
  span <- getField (decodeSourceSpan path) obj "sourceSpan"
  imports <- getField (decodeArray (decodeImport (decodeAnn' path))) obj "imports"
  exports <- getField (decodeArray decodeIdent) obj "exports"
  reExports <- getField decodeReExports obj "reExports"
  decls <- getField (decodeArray (decodeBind (decodeAnn' path))) obj "decls"
  foreign_ <- getField (decodeArray decodeIdent) obj "foreign"
  comments <- getField (decodeArray decodeComment) obj "comments"
  pure $ Module
    { name
    , path
    , span
    , imports
    , exports
    , reExports
    , decls: decls
    , foreign: foreign_
    , comments
    }

decodeReExports :: Json -> JsonDecode (Array ReExport)
decodeReExports json = do
  obj <- decodeJObject json
  all <- traverse (traverse (decodeArray decodeIdent)) $ Object.toArrayWithKey Tuple obj
  pure $ all >>= \(Tuple mn idents) -> ReExport (ModuleName mn) <$> idents

decodeBind :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Bind a)
decodeBind decAnn json = do
  obj <- decodeJObject json
  typ <- getField decodeString obj "bindType"
  case typ of
    "NonRec" -> NonRec <$> decodeBinding decAnn obj
    "Rec" -> Rec <$> getField (decodeArray (decodeJObject >=> decodeBinding decAnn)) obj "binds"
    _ -> throwError $ TypeMismatch "Bind"

decodeBinding :: forall a. (Json -> JsonDecode a) -> Object Json -> JsonDecode (Binding a)
decodeBinding decAnn obj = do
  ann <- getField decAnn obj "annotation"
  ident <- getField decodeIdent obj "identifier"
  expr <- getField (decodeExpr decAnn) obj "expression"
  pure $ Binding ann ident expr

decodeExpr :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Expr a)
decodeExpr decAnn json = do
  obj <- decodeJObject json
  ann <- getField decAnn obj "annotation"
  typ <- getField decodeString obj "type"
  case typ of
    "Var" ->
      ExprVar ann <$> getField (decodeQualified decodeIdent) obj "value"
    "Literal" ->
      ExprLit ann <$> getField (decodeLiteral (decodeExpr decAnn)) obj "value"
    "Constructor" -> do
      tyn <- getField decodeProperName obj "typeName"
      con <- getField decodeIdent obj "constructorName"
      is  <- getField (decodeArray decodeString) obj "fieldNames"
      pure $ ExprConstructor ann tyn con is
    "Accessor" -> do
      e <- getField (decodeExpr decAnn) obj "expression"
      f <- getField decodeString obj "fieldName"
      pure $ ExprAccessor ann e f
    "ObjectUpdate" -> do
      e <- getField (decodeExpr decAnn) obj "expression"
      us <- getField (decodeRecord (decodeExpr decAnn)) obj "updates"
      pure $ ExprUpdate ann e us
    "Abs" -> do
      idn <- getField decodeIdent obj "argument"
      e <- getField (decodeExpr decAnn) obj "body"
      pure $ ExprAbs ann idn e
    "App" -> do
      e1 <- getField (decodeExpr decAnn) obj "abstraction"
      e2 <- getField (decodeExpr decAnn) obj "argument"
      pure $ ExprApp ann e1 e2
    "Case" -> do
      cs <- getField (decodeArray (decodeExpr decAnn)) obj "caseExpressions"
      cas <- getField (decodeArray (decodeCaseAlternative decAnn)) obj "caseAlternatives"
      pure $ ExprCase ann cs cas
    "Let" -> do
      bs <- getField (decodeArray (decodeBind decAnn)) obj "binds"
      e <- getField (decodeExpr decAnn) obj "expression"
      pure $ ExprLet ann bs e
    _ ->
      throwError $ TypeMismatch "Expr"

decodeCaseAlternative :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (CaseAlternative a)
decodeCaseAlternative decAnn json = do
  obj <- decodeJObject json
  binders <- getField (decodeArray (decodeBinder decAnn)) obj "binders"
  isGuarded <- getField decodeBoolean obj "isGuarded"
  if isGuarded then do
    es <- getField (decodeArray (decodeGuard decAnn)) obj "expressions"
    pure $ CaseAlternative binders (Guarded es)
  else do
    e <- getField (decodeExpr decAnn) obj "expression"
    pure $ CaseAlternative binders (Unconditional e)

decodeGuard :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Guard a)
decodeGuard decAnn json = do
  obj <- decodeJObject json
  guard <- getField (decodeExpr decAnn) obj "guard"
  expr <- getField (decodeExpr decAnn) obj "expression"
  pure $ Guard guard expr

decodeBinder :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Binder a)
decodeBinder decAnn json = do
  obj <- decodeJObject json
  ann <- getField decAnn obj "annotation"
  typ <- getField decodeString obj "binderType"
  case typ of
    "NullBinder" -> 
      pure $ BinderNull ann
    "VarBinder" ->
      BinderVar ann <$> getField decodeIdent obj "identifier"
    "LiteralBinder" -> 
      BinderLit ann <$> getField (decodeLiteral (decodeBinder decAnn)) obj "literal"
    "ConstructorBinder" -> do
      tyn <- getField (decodeQualified decodeProperName) obj "typeName"
      ctn <- getField (decodeQualified decodeIdent) obj "constructorName"
      binders <- getField (decodeArray (decodeBinder decAnn)) obj "binders"
      pure $ BinderConstructor ann tyn ctn binders
    "NamedBinder" -> do
      ident <- getField decodeIdent obj "identifier"
      binder <- getField (decodeBinder decAnn) obj "binder"
      pure $ BinderNamed ann ident binder
    _ ->
      throwError $ TypeMismatch "Binder"

decodeLiteral :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Literal a)
decodeLiteral dec json = do
  obj <- decodeJObject json
  typ <- getField decodeString obj "literalType"
  case typ of
    "IntLiteral" ->
      LitInt <$> getField decodeInt obj "value"
    "NumberLiteral" ->
      LitNumber <$> getField decodeNumber obj "value"
    "StringLiteral" -> 
      LitString <$> getField decodeString obj "value"
    "CharLiteral" -> do
      str <- getField decodeString obj "value"
      LitChar <$> note (TypeMismatch "Char") do
        guard (SCU.length str == 1)
        Array.head $ SCU.toCharArray str
    "BooleanLiteral" ->
      LitBoolean <$> getField decodeBoolean obj "value"
    "ArrayLiteral" -> 
      LitArray <$> getField (decodeArray dec) obj "value"
    "ObjectLiteral" ->
      LitRecord <$> getField (decodeRecord dec) obj "value"
    _ ->
      throwError $ TypeMismatch "Literal"

decodeRecord :: forall a. (Json -> JsonDecode a) -> Json -> JsonDecode (Array (Prop a))
decodeRecord = decodeArray <<< map (map (uncurry Prop)) <<< decodeTuple decodeString

decodeComment :: Json -> JsonDecode Comment
decodeComment json = do
  obj <- decodeJObject json
  LineComment <$> getField decodeString obj "LineComment"
    <|> BlockComment <$> getField decodeString obj "BlockComment"