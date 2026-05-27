module PureScript.Backend.Optimizer.Codegen.EcmaScript.Common
  ( esAccessor
  , esApp
  , esAssign
  , esBoolean
  , esComment
  , esEscapeIdent
  , esEscapeProp
  , esEscapeSpecial
  , esEscapeString
  , esIndex
  , esInt
  , esModuleName
  , esNumber
  , esString
  , esTernary
  ) where

import Prelude

import Data.Array (fold)
import Data.Array as Array
import Data.Enum (fromEnum)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, noFlags, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Optimizer.CoreFn (Comment(..), ModuleName(..))

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
  Regex.replace' (unsafeRegex """(?:^[^\p{L}_$])|(?:[^\p{L}0-9_$])""" (unicode <> global)) \m _ ->
    case m of
      "'" -> "$p"
      "." -> "$d"
      _ -> "$x" <> String.joinWith "" (show <<< fromEnum <$> String.toCodePointArray m)

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

esAssign :: forall a. Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a
esAssign ident b = Dodo.words
  [ ident
  , Dodo.text "="
  , b
  ]

esAccessor :: forall a. Dodo.Doc a -> String -> Dodo.Doc a
esAccessor expr prop = case esEscapeProp prop of
  Nothing ->
    expr <> Dodo.text "." <> Dodo.text prop
  Just escaped ->
    expr <> Dodo.Common.jsSquares (Dodo.text escaped)

esIndex :: forall a. Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a
esIndex expr ix = expr <> Dodo.text "[" <> ix <> Dodo.text "]"

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

esComment :: forall a. Comment -> Dodo.Doc a
esComment = case _ of
  LineComment str ->
    Dodo.text "//" <> Dodo.text str
  BlockComment str ->
    Dodo.text "/*" <> Dodo.text str <> Dodo.text "*/"

esTernary :: forall a. Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a
esTernary a b c =
  Dodo.flexGroup $ fold
    [ a
    , Dodo.spaceBreak
    , Dodo.indent $ fold
        [ Dodo.text "?"
        , Dodo.space
        , Dodo.alignCurrentColumn b
        , Dodo.spaceBreak
        , Dodo.text ":"
        , Dodo.space
        , Dodo.alignCurrentColumn c
        ]
    ]

-- Mirrors `prettyPrintStringJS` in the purescript compiler:
-- https://github.com/purescript/purescript/blob/master/src/Language/PureScript/PSString.hs#L200-L214
-- Every code unit > 0xFF is escaped as \uXXXX, every other non-printable
-- ASCII as \xXX. This catches Unicode noncharacters (U+FFFE, U+FFFF,
-- U+FDD0..U+FDEF) that Chrome MV3 rejects in extension scripts, and avoids
-- a JSON round-trip.
esEscapeString :: String -> String
esEscapeString s = "\"" <> foldMap encode (SCU.toCharArray s) <> "\""
  where
  encode c = case fromEnum c of
    0x08 -> "\\b"
    0x09 -> "\\t"
    0x0A -> "\\n"
    0x0B -> "\\v"
    0x0C -> "\\f"
    0x0D -> "\\r"
    0x22 -> "\\\""
    0x5C -> "\\\\"
    n
      | n > 0xFF -> "\\u" <> padHex 4 n
      | n < 0x20 || n > 0x7E -> "\\x" <> padHex 2 n
      | otherwise -> SCU.singleton c
  padHex width n = leftPadZero width (Int.toStringAs Int.hexadecimal n)
  leftPadZero width str
    | String.length str >= width = str
    | otherwise = leftPadZero width ("0" <> str)
