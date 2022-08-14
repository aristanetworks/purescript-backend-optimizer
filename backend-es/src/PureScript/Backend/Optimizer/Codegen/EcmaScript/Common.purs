module PureScript.Backend.Optimizer.Codegen.EcmaScript.Common where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), uncurry)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Optimizer.CoreFn (Comment(..), Ident(..), ModuleName(..), Prop(..))

data EsStatement a
  = Statement a
  | Control a
  | Return a
  | ReturnObject a

esIdent :: forall a. Ident -> Dodo.Doc a
esIdent (Ident a) = Dodo.text (esEscapeIdent a)

esModuleName :: forall a. ModuleName -> Dodo.Doc a
esModuleName (ModuleName mn) = Dodo.text (esEscapeIdent mn)

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
esFwdRef ident = Dodo.text "let" <> Dodo.space <> esIdent ident

esLetBinding :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
esLetBinding ident b = Dodo.words
  [ Dodo.text "let"
  , esIdent ident
  , Dodo.text "="
  , b
  ]

esLetBindings :: forall a. NonEmptyArray (Tuple Ident (Maybe (Dodo.Doc a))) -> Dodo.Doc a
esLetBindings bs = do
  let kw = Dodo.text "let"
  let sep = Dodo.flexAlt (Dodo.text ", ") (Dodo.text ";" <> Dodo.break <> kw <> Dodo.space)
  Dodo.flexGroup $ Dodo.words
    [ kw
    , Dodo.foldWithSeparator sep $ map
        ( \(Tuple ident mb) ->
            case mb of
              Nothing ->
                esIdent ident
              Just b ->
                esAssign ident b
        )
        bs
    ]

esBinding :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
esBinding ident b = Dodo.words
  [ Dodo.text "const"
  , esIdent ident
  , Dodo.text "="
  , b
  ]

esBindings :: forall a. NonEmptyArray (Tuple Ident (Dodo.Doc a)) -> Dodo.Doc a
esBindings bs = do
  let kw = Dodo.text "const"
  let sep = Dodo.flexAlt (Dodo.text ", ") (Dodo.text ";" <> Dodo.break <> kw <> Dodo.space)
  Dodo.flexGroup $ Dodo.words
    [ kw
    , Dodo.foldWithSeparator sep (uncurry esAssign <$> bs)
    ]

esAssign :: forall a. Ident -> Dodo.Doc a -> Dodo.Doc a
esAssign ident b = Dodo.words
  [ esIdent ident
  , Dodo.text "="
  , b
  ]

esAssignRef :: forall a. Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a
esAssignRef lhs rhs = fold
  [ lhs
  , Dodo.text ".value"
  , Dodo.space
  , Dodo.text "="
  , Dodo.space
  , rhs
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
      foldMap esIdent args
    else
      Dodo.Common.jsParens (Dodo.foldWithSeparator Dodo.Common.trailingComma (esIdent <$> args))
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
    [ case String.stripPrefix (String.Pattern "$__unused") (unwrap arg) of
        Nothing ->
          esIdent arg
        Just _ ->
          Dodo.text "()"
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
    Just $ esEscapeString prop
  where
  safeRegex = unsafeRegex """^[a-zA-Z_$][a-zA-Z0-9_$]*$""" noFlags

esString :: forall a. String -> Dodo.Doc a
esString = Dodo.text <<< esEscapeString

esNumber :: forall a. Number -> Dodo.Doc a
esNumber = Dodo.text <<< show

esInt :: forall a. Int -> Dodo.Doc a
esInt = Dodo.text <<< show

esChar :: forall a. Char -> Dodo.Doc a
esChar = Dodo.text <<< esEscapeString <<< SCU.singleton

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

esBranches :: forall a. NonEmptyArray (Tuple (Dodo.Doc a) (Array (EsStatement (Dodo.Doc a)))) -> Maybe (Array (EsStatement (Dodo.Doc a))) -> Dodo.Doc a
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
  , esModuleName mn
  , Dodo.text "from"
  , esString path
  ]

esImports :: forall a. String -> Array (Tuple Ident Ident) -> Dodo.Doc a
esImports path imports = Dodo.words
  [ Dodo.text "import"
  , Dodo.Common.jsCurlies $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ map
      ( \(Tuple id1 id2) -> do
          let id1' = esEscapeIdent (unwrap id2)
          let id2' = esEscapeSpecial (unwrap id1)
          if id1' == id2' then
            Dodo.text id1'
          else
            Dodo.words
              [ Dodo.text id2'
              , Dodo.text "as"
              , Dodo.text id1'
              ]
      )
      imports
  , Dodo.text "from"
  , esString path
  ]

esExports :: forall a. Maybe String -> Array (Tuple Ident Ident) -> Dodo.Doc a
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
      , esString path
      ]
  ]

esExportAllFrom :: forall a. String -> Dodo.Doc a
esExportAllFrom path = Dodo.words
  [ Dodo.text "export"
  , Dodo.text "*"
  , Dodo.text "from"
  , esString path
  ]

esUndefined :: forall a. Dodo.Doc a
esUndefined = Dodo.text "undefined"

esError :: forall a. String -> Dodo.Doc a
esError str = Dodo.words
  [ Dodo.text "throw"
  , Dodo.text "new"
  , esApp (Dodo.text "Error") [ esString str ]
  ]

esContinue :: forall a. Dodo.Doc a
esContinue = Dodo.text "continue"

esSepStatements :: forall f a. Foldable f => f (Dodo.Doc a) -> Dodo.Doc a
esSepStatements = Dodo.foldWithSeparator (Dodo.text ";" <> Dodo.break)

esPure :: forall a. Dodo.Doc a -> Dodo.Doc a
esPure doc = Dodo.text "/* #__PURE__ */" <> Dodo.space <> doc

esComment :: forall a. Comment -> Dodo.Doc a
esComment = case _ of
  LineComment str ->
    Dodo.text "//" <> Dodo.text str
  BlockComment str ->
    Dodo.text "/*" <> Dodo.text str <> Dodo.text "*/"

esEscapeString :: String -> String
esEscapeString = Json.stringify <<< Json.fromString
