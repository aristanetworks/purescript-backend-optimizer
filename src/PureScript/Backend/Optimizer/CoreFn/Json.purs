-- @inline JSON.case_ always
module PureScript.Backend.Optimizer.CoreFn.Json
  ( decodeModule
  , decodeModule'
  , decodeAnn
  , JsonDecodeError
  , printJsonDecodeError
  ) where

import Prelude hiding (bind)

import Control.Alternative (guard)
import Control.Monad.Error.Class (throwError)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either (Either(..), note)
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JSON (JObject, JSON, case_, isNull)
import JSON.Array as JA
import JSON.Object as Object
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude
import PureScript.Backend.Optimizer.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Comment(..), ConstructorType(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Meta(..), Module(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..), ReExport(..), SourcePos, SourceSpan, emptySpan)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

data JsonDecodeError
  = TypeMismatch String
  | AtIndex Int JsonDecodeError
  | AtKey String JsonDecodeError
  | MissingValue

type JsonDecode = Either JsonDecodeError

infixr 2 alt as <|>

alt :: forall e a. Either e a -> (Unit -> Either e a) -> Either e a
alt a k = case a of
  Left _ -> k unit
  Right _ -> a

-- Either's bind implementation is not ideal from an optimization
-- standpoint and generates awkward code.
bind :: forall e a b. Either e a -> (a -> Either e b) -> Either e b
bind a k = case a of
  Left err ->
    Left err
  Right a' ->
    k a'

decodeSourcePos :: JSON -> JsonDecode SourcePos
decodeSourcePos json = do
  res <- decodeJArray json
  case res of
    [ l, c ] -> do
      line <- decodeInt l
      column <- decodeInt c
      pure { line, column }
    _ -> Left $ TypeMismatch "SourcePos"

decodeSourceSpan :: String -> JSON -> JsonDecode SourceSpan
decodeSourceSpan path json = do
  obj <- decodeJObject json
  start <- getField decodeSourcePos obj "start"
  end <- getField decodeSourcePos obj "end"
  pure { path, start, end }

decodeConstructorType :: JSON -> JsonDecode ConstructorType
decodeConstructorType json = do
  str <- decodeString json
  case str of
    "ProductType" -> pure ProductType
    "SumType" -> pure SumType
    _ -> throwError $ TypeMismatch "ConstructorType"

decodeIdent :: JSON -> JsonDecode Ident
decodeIdent = coerce decodeString

decodeProperName :: JSON -> JsonDecode ProperName
decodeProperName = coerce decodeString

decodeModuleName :: JSON -> JsonDecode ModuleName
decodeModuleName = map (ModuleName <<< intercalate ".") <<< decodeArray decodeString

decodeQualified :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Qualified a)
decodeQualified k json = do
  obj <- decodeJObject json
  moduleName <- getFieldOptional' decodeModuleName obj "moduleName"
  identifier <- getField k obj "identifier"
  pure $ Qualified moduleName identifier

decodeMeta :: JSON -> JsonDecode Meta
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
    "IsSyntheticApp" ->
      pure IsSyntheticApp
    _ ->
      throwError $ TypeMismatch "Meta"

decodeAnn :: String -> JSON -> JsonDecode Ann
decodeAnn _path json = do
  obj <- decodeJObject json
  -- Currently disabled because spans are not used and are a performance drain.
  -- span <- getField (decodeSourceSpan path) obj "sourceSpan"
  meta <- getFieldOptional' decodeMeta obj "meta"
  pure $ Ann { span: emptySpan, meta }

decodeImport :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Import a)
decodeImport decodeAnn' json = do
  obj <- decodeJObject json
  ann <- getField decodeAnn' obj "annotation"
  mod <- getField decodeModuleName obj "moduleName"
  pure $ Import ann mod

decodeModule :: JSON -> JsonDecode (Module Ann)
decodeModule = decodeModule' decodeAnn

decodeModule' :: forall a. (String -> JSON -> JsonDecode a) -> JSON -> JsonDecode (Module a)
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

decodeReExports :: JSON -> JsonDecode (Array ReExport)
decodeReExports json = do
  obj <- decodeJObject json
  all <- traverse (traverse (decodeArray decodeIdent)) $ Object.toUnfoldable obj
  pure $ all >>= \(Tuple mn idents) -> ReExport (ModuleName mn) <$> idents

decodeBind :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Bind a)
decodeBind decAnn json = do
  obj <- decodeJObject json
  typ <- getField decodeString obj "bindType"
  case typ of
    "NonRec" -> NonRec <$> decodeBinding decAnn obj
    "Rec" -> Rec <$> getField (decodeArray (decodeJObject >=> decodeBinding decAnn)) obj "binds"
    _ -> throwError $ TypeMismatch "Bind"

decodeBinding :: forall a. (JSON -> JsonDecode a) -> JObject -> JsonDecode (Binding a)
decodeBinding decAnn obj = do
  ann <- getField decAnn obj "annotation"
  ident <- getField decodeIdent obj "identifier"
  expr <- getField (decodeExpr decAnn) obj "expression"
  pure $ Binding ann ident expr

decodeExpr :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Expr a)
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
      is <- getField (decodeArray decodeString) obj "fieldNames"
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

decodeCaseAlternative :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (CaseAlternative a)
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

decodeGuard :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Guard a)
decodeGuard decAnn json = do
  obj <- decodeJObject json
  guard <- getField (decodeExpr decAnn) obj "guard"
  expr <- getField (decodeExpr decAnn) obj "expression"
  pure $ Guard guard expr

decodeBinder :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Binder a)
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

decodeLiteral :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Literal a)
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

decodeRecord :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Array (Prop a))
decodeRecord = decodeArray <<< decodeProp
  where
  decodeProp decoder json = do
    arr <- decodeJArray json
    case arr of
      [ a, b ] -> do
        prop <- decodeString a
        value <- decoder b
        pure $ Prop prop value
      _ ->
        Left $ TypeMismatch "Tuple"

decodeComment :: JSON -> JsonDecode Comment
decodeComment json = do
  obj <- decodeJObject json
  LineComment <$> getField decodeString obj "LineComment"
    <|> \_ -> BlockComment <$> getField decodeString obj "BlockComment"

decodeArray :: forall a. (JSON -> JsonDecode a) -> JSON -> JsonDecode (Array a)
decodeArray decoder json = case decodeJArray json of
  Left err ->
    Left err
  Right arr -> ST.run Prelude.do
    out <- STArray.new
    ix <- STRef.new 0
    con <- STRef.new true
    res <- STRef.new (unsafeCoerce unit)
    let len = Array.length arr
    ST.while (STRef.read con) Prelude.do
      ix' <- STRef.read ix
      if ix' == len then Prelude.do
        out' <- STArray.unsafeFreeze out
        _ <- STRef.write false con
        _ <- STRef.write (Right out') res
        pure unit
      else
        case decoder (unsafePartial (Array.unsafeIndex arr ix')) of
          Left err -> Prelude.do
            _ <- STRef.write false con
            _ <- STRef.write (Left (AtIndex ix' err)) res
            pure unit
          Right val -> Prelude.do
            _ <- STArray.push val out
            _ <- STRef.write (ix' + 1) ix
            pure unit
    STRef.read res

getField :: forall a. (JSON -> JsonDecode a) -> JObject -> String -> JsonDecode a
getField decode obj prop =
  case Object.lookup prop obj of
    Nothing ->
      Left $ AtKey prop MissingValue
    Just json ->
      decode json

getFieldOptional' :: forall a. (JSON -> JsonDecode a) -> JObject -> String -> JsonDecode (Maybe a)
getFieldOptional' decode obj prop = do
  case Object.lookup prop obj of
    Nothing ->
      Right Nothing
    Just json
      | isNull json ->
          Right Nothing
      | otherwise ->
          Just <$> decode json

decodeJObject :: JSON -> JsonDecode JObject
decodeJObject = case_ fail fail fail fail fail Right
  where
  fail :: forall a. a -> JsonDecode JObject
  fail _ = Left $ TypeMismatch "Object"

decodeJArray :: JSON -> JsonDecode (Array JSON)
decodeJArray = case_ fail fail fail fail (Right <<< JA.toUnfoldable) fail
  where
  fail :: forall a. a -> JsonDecode (Array JSON)
  fail _ = Left $ TypeMismatch "Array"

decodeString :: JSON -> JsonDecode String
decodeString = case_ fail fail fail Right fail fail
  where
  fail :: forall a. a -> JsonDecode String
  fail _ = Left $ TypeMismatch "String"

decodeNumber :: JSON -> JsonDecode Number
decodeNumber = case_ fail fail Right fail fail fail
  where
  fail :: forall a. a -> JsonDecode Number
  fail _ = Left $ TypeMismatch "Number"

decodeBoolean :: JSON -> JsonDecode Boolean
decodeBoolean = case_ fail Right fail fail fail fail
  where
  fail :: forall a. a -> JsonDecode Boolean
  fail _ = Left $ TypeMismatch "Boolean"

decodeInt :: JSON -> JsonDecode Int
decodeInt json = do
  num <- decodeNumber json
  case Int.fromNumber num of
    Nothing ->
      Left $ TypeMismatch "Int"
    Just int ->
      Right int

printJsonDecodeError :: JsonDecodeError -> String
printJsonDecodeError err =
  "An error occurred while decoding a JSON value:\n" <> go err
  where
  go = case _ of
    TypeMismatch ty -> "  Expected value of type '" <> ty <> "'."
    AtIndex ix inner -> "  At array index " <> show ix <> ":\n" <> go inner
    AtKey key inner -> "  At object key \'" <> key <> "\':\n" <> go inner
    MissingValue -> "  No value was found."

