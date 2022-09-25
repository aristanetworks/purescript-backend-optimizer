module PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (class Foldable, fold, foldMap, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Common (esAccessor, esApp, esAssign, esBoolean, esEscapeProp, esIdent, esIndex, esInt, esModuleName, esNumber, esString)
import PureScript.Backend.Optimizer.CoreFn (Ident, ModuleName, Qualified(..))

data ESSyntax a
  = ESString String
  | ESNumber Number
  | ESInt Int
  | ESBoolean Boolean
  | ESArray (Array (ESArrayElement a))
  | ESObject (Array (ESObjectElement a))
  | ESAccess a String
  | ESIndex a Int
  | ESIdent (Qualified Ident)
  | ESRuntime (ESRuntimeOp a)
  | ESCall a (Array a)
  | ESBinary ESBinaryOp a a
  | ESUnary ESUnaryOp a
  | ESAssign ESAssignment a
  | ESArrowFunction (Array Ident) (Array a)
  | ESConst (NonEmptyArray (Tuple Ident a))
  | ESLet (NonEmptyArray (Tuple Ident (Maybe a)))
  | ESIfElse a (Array a) (Array a)
  | ESWhile a (Array a)
  | ESReturn a
  | ESContinue
  | ESUndefined

derive instance Functor ESSyntax

instance Foldable ESSyntax where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    ESString _ -> mempty
    ESNumber _ -> mempty
    ESInt _ -> mempty
    ESBoolean _ -> mempty
    ESArray as -> foldMap (foldMap f) as
    ESObject as -> foldMap (foldMap f) as
    ESAccess a _ -> f a
    ESIndex a _ -> f a
    ESIdent _ -> mempty
    ESRuntime a -> foldMap f a
    ESCall a bs -> f a <> foldMap f bs
    ESBinary _ a b -> f a <> f b
    ESUnary _ a -> f a
    ESAssign _ a -> f a
    ESArrowFunction _ as -> foldMap f as
    ESConst as -> foldMap (foldMap f) as
    ESLet as -> foldMap (foldMap (foldMap f)) as
    ESIfElse a bs cs -> f a <> foldMap f bs <> foldMap f cs
    ESWhile a bs -> f a <> foldMap f bs
    ESReturn a -> f a
    ESContinue -> mempty
    ESUndefined -> mempty

data ESAssignment
  = ESAssignIdent Ident
  | ESAssignAccess ESAssignment String

data ESArrayElement a
  = ESArrayValue a
  | ESArraySpread a

derive instance Functor ESArrayElement

instance Foldable ESArrayElement where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    ESArrayValue a -> f a
    ESArraySpread a -> f a

data ESObjectElement a
  = ESObjectPun String
  | ESObjectField String a
  | ESObjectSpread a

derive instance Functor ESObjectElement

instance Foldable ESObjectElement where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    ESObjectPun _ -> mempty
    ESObjectField _ a -> f a
    ESObjectSpread a -> f a

data ESBinaryOp
  = ESOr
  | ESAnd
  | ESLessThan
  | ESLessThanEqual
  | ESGreaterThan
  | ESGreaterThanEqual
  | ESAdd
  | ESSubtract
  | ESDivide
  | ESMultiply
  | ESBitAnd
  | ESBitOr
  | ESBitShiftLeft
  | ESBitShitRight
  | ESBitXor
  | ESZeroFillShiftRight
  | ESEquals
  | ESNotEquals

data ESUnaryOp
  = ESNot
  | ESNegate
  | ESBitNegate

data ESPrec
  = ESPrecStatement
  | ESPrecControl
  | ESPrecArrow
  | ESPrecAssign
  | ESPrecBinary Int
  | ESPrecPrefix
  | ESPrecCall
  | ESPrecAtom

derive instance Eq ESPrec
derive instance Ord ESPrec

precOf :: forall a. ESSyntax a -> ESPrec
precOf = case _ of
  ESString _ -> ESPrecAtom
  ESNumber _ -> ESPrecAtom
  ESInt _ -> ESPrecAtom
  ESBoolean _ -> ESPrecAtom
  ESArray _ -> ESPrecAtom
  ESObject _ -> ESPrecAtom
  ESAccess _ _ -> ESPrecCall
  ESIndex _ _ -> ESPrecCall
  ESIdent _ -> ESPrecAtom
  ESRuntime _ -> ESPrecCall
  ESCall _ _ -> ESPrecCall
  ESBinary op _ _ -> ESPrecBinary (precOfOp op)
  ESUnary _ _ -> ESPrecPrefix
  ESAssign _ _ -> ESPrecAssign
  ESArrowFunction _ _ -> ESPrecArrow
  ESConst _ -> ESPrecStatement
  ESLet _ -> ESPrecStatement
  ESIfElse _ _ _ -> ESPrecControl
  ESWhile _ _ -> ESPrecControl
  ESReturn _ -> ESPrecStatement
  ESContinue -> ESPrecStatement
  ESUndefined -> ESPrecAtom
  where
  precOfOp = case _ of
    ESOr -> 1
    ESAnd -> 2
    ESBitOr -> 3
    ESBitXor -> 4
    ESBitAnd -> 5
    ESEquals -> 6
    ESNotEquals -> 6
    ESLessThan -> 7
    ESLessThanEqual -> 7
    ESGreaterThan -> 7
    ESGreaterThanEqual -> 7
    ESBitShiftLeft -> 8
    ESBitShitRight -> 8
    ESZeroFillShiftRight -> 8
    ESAdd -> 9
    ESSubtract -> 9
    ESDivide -> 10
    ESMultiply -> 10

data ESRuntimeOp a
  = ESBinding a
  | ESFail

derive instance Functor ESRuntimeOp

instance Foldable ESRuntimeOp where
  foldMap f = case _ of
    ESBinding a -> f a
    ESFail -> mempty
  foldr a = foldrDefault a
  foldl a = foldlDefault a

data ESExpr = ESExpr ESAnalysis (ESSyntax ESExpr)

newtype ESAnalysis = ESAnalysis
  { deps :: Set ModuleName
  , runtime :: Boolean
  }

instance Semigroup ESAnalysis where
  append (ESAnalysis a) (ESAnalysis b) = ESAnalysis
    { deps: a.deps <> b.deps
    , runtime: a.runtime || b.runtime
    }

instance Monoid ESAnalysis where
  mempty = ESAnalysis { deps: mempty, runtime: false }

needsDep :: ModuleName -> ESAnalysis -> ESAnalysis
needsDep mn (ESAnalysis a) = ESAnalysis a { deps = Set.insert mn a.deps }

needsRuntime :: ESAnalysis -> ESAnalysis
needsRuntime (ESAnalysis a) = ESAnalysis a { runtime = true }

esAnalysisOf :: ESExpr -> ESAnalysis
esAnalysisOf (ESExpr a _) = a

build :: ESSyntax ESExpr -> ESExpr
build syn = case syn of
  ESIdent (Qualified (Just mn) _) ->
    ESExpr (needsDep mn mempty) syn
  ESRuntime op ->
    ESExpr (needsRuntime (foldMap esAnalysisOf op)) syn
  _ ->
    ESExpr (foldMap esAnalysisOf syn) syn

class HasSyntax a where
  syntaxOf :: a -> ESSyntax a

wrapPrec :: forall a. ESPrec -> Tuple ESPrec (Dodo.Doc a) -> Dodo.Doc a
wrapPrec p1 (Tuple p2 doc)
  | p2 > p1 = Dodo.Common.jsParens doc
  | otherwise = doc

printSyntax :: forall x a. HasSyntax x => ESSyntax x -> Tuple ESPrec (Dodo.Doc a)
printSyntax syn = case syn of
  ESString str ->
    Tuple ESPrecAtom $ esString str
  ESNumber num ->
    Tuple ESPrecAtom $ esNumber num
  ESInt int ->
    Tuple ESPrecAtom $ esInt int
  ESBoolean bool ->
    Tuple ESPrecAtom $ esBoolean bool
  ESArray as ->
    Tuple ESPrecAtom $ printArray as
  ESObject as ->
    Tuple ESPrecAtom $ printObject as
  ESIdent (Qualified mb ident) ->
    case mb of
      Nothing ->
        Tuple ESPrecAtom $ esIdent ident
      Just mn ->
        Tuple ESPrecCall $ esModuleName mn <> Dodo.text "." <> esIdent ident
  ESAccess a prop -> do
    let p1 = ESPrecCall
    let a' = wrapPrec p1 (printSyntax (syntaxOf a))
    Tuple p1 $ esAccessor a' prop
  ESIndex a ix -> do
    let p1 = ESPrecCall
    let a' = wrapPrec p1 (printSyntax (syntaxOf a))
    Tuple p1 $ esIndex a' ix
  ESRuntime op ->
    Tuple ESPrecCall $ case op of
      ESBinding a ->
        esApp (Dodo.text "$runtime.binding") [ snd (printSyntax (syntaxOf a)) ]
      ESFail ->
        Dodo.text "$runtime.fail()"
  ESCall a bs -> do
    let p1 = ESPrecCall
    let a' = wrapPrec p1 (printSyntax (syntaxOf a))
    Tuple ESPrecCall $ esApp a' (snd <<< printSyntax <<< syntaxOf <$> bs)
  ESBinary op a b -> do
    let Tuple pn str = printESBinaryOp op
    let p1 = ESPrecBinary pn
    let a' = printSyntax (syntaxOf a)
    let b' = printSyntax (syntaxOf b)
    Tuple p1 $ Dodo.words [ wrapPrec p1 a', Dodo.text str, wrapPrec p1 b' ]
  ESUnary op a -> do
    let p1 = ESPrecPrefix
    let a' = wrapPrec p1 (printSyntax (syntaxOf a))
    Tuple p1 $  Dodo.words [ Dodo.text (printESUnaryOp op), a' ]
  ESAssign asn b ->
    Tuple ESPrecStatement $ Dodo.words [ printAssignment asn, Dodo.text "=", snd (printSyntax (syntaxOf b)) ]
  ESArrowFunction args a ->
    Tuple ESPrecArrow $ printArrowFunction args a
  ESConst bindings ->
    Tuple ESPrecStatement $ printConst bindings
  ESLet bindings ->
    Tuple ESPrecStatement $ printLet bindings
  ESIfElse a bs cs ->
    Tuple ESPrecControl $ printIfElse a bs cs
  ESWhile a bs ->
    Tuple ESPrecControl $ printWhile a bs
  ESReturn a ->
    Tuple ESPrecStatement $ Dodo.words [ Dodo.text "return", snd (printSyntax (syntaxOf a)) ]
  ESContinue ->
    Tuple ESPrecStatement $ Dodo.text "continue"
  ESUndefined ->
    Tuple ESPrecAtom $ Dodo.text "undefined"

printESBinaryOp :: ESBinaryOp -> Tuple Int String
printESBinaryOp = case _ of
  ESOr -> Tuple 1 "||"
  ESAnd -> Tuple 2 "&&"
  ESBitOr -> Tuple 3 "|"
  ESBitXor -> Tuple 4 "^"
  ESBitAnd -> Tuple 5 "&"
  ESEquals -> Tuple 6 "==="
  ESNotEquals -> Tuple 6 "!=="
  ESLessThan -> Tuple 7 "<"
  ESLessThanEqual -> Tuple 7 "<="
  ESGreaterThan -> Tuple 7 ">"
  ESGreaterThanEqual -> Tuple 7 ">="
  ESBitShiftLeft -> Tuple 8 "<<"
  ESBitShitRight -> Tuple 8 ">>"
  ESZeroFillShiftRight -> Tuple 8 ">>>"
  ESAdd -> Tuple 9 "+"
  ESSubtract -> Tuple 9 "-"
  ESDivide -> Tuple 10 "/"
  ESMultiply -> Tuple 10 "*"

printESUnaryOp :: ESUnaryOp -> String
printESUnaryOp = case _ of
  ESNot -> "!"
  ESNegate -> "-"
  ESBitNegate -> "~"

printArrayElement :: forall x a. HasSyntax x => ESArrayElement x -> Dodo.Doc a
printArrayElement = case _ of
  ESArrayValue a ->
    snd (printSyntax (syntaxOf a))
  ESArraySpread a ->
    snd (printSyntax (syntaxOf a)) <> Dodo.text "..."

printObjectElement :: forall x a. HasSyntax x => ESObjectElement x -> Dodo.Doc a
printObjectElement = case _ of
  ESObjectPun field ->
    Dodo.text (fromMaybe field (esEscapeProp field))
  ESObjectField field a ->
    Dodo.text (fromMaybe field (esEscapeProp field))
      <> Dodo.text ":"
      <> Dodo.space
      <> snd (printSyntax (syntaxOf a))
  ESObjectSpread a ->
    snd (printSyntax (syntaxOf a)) <> Dodo.text "..."

printArray :: forall x a. HasSyntax x => Array (ESArrayElement x) -> Dodo.Doc a
printArray =
  Dodo.Common.jsSquares
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map printArrayElement

printObject :: forall x a. HasSyntax x => Array (ESObjectElement x) -> Dodo.Doc a
printObject =
  Dodo.Common.jsCurlies
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map printObjectElement

printAssignment :: forall a. ESAssignment -> Dodo.Doc a
printAssignment = case _ of
  ESAssignIdent ident ->
    esIdent ident
  ESAssignAccess a prop ->
    esAccessor (printAssignment a) prop

printArrowFunction :: forall x a. HasSyntax x => Array Ident -> Array x -> Dodo.Doc a
printArrowFunction args stmts = Dodo.words
  [ if Array.length args == 1 then
      foldMap esIdent args
    else
      Dodo.Common.jsParens $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ esIdent <$> args
  , Dodo.text "=>"
  , case stmts of
      [ stmt ] | ESReturn a <- syntaxOf stmt ->
        case syntaxOf a of
          ESObject as ->
            Dodo.Common.jsParens $ printObject as
          other ->
            snd $ printSyntax other
      _ ->
        Dodo.Common.jsCurlies $ Dodo.lines $ printStatement <$> stmts
  ]

printStatement :: forall x a. HasSyntax x => x -> Dodo.Doc a
printStatement x = do
  let Tuple prec doc = printSyntax (syntaxOf x)
  case prec of
    ESPrecControl ->
      doc
    _ ->
      doc <> Dodo.text ";"

printLet :: forall x a. HasSyntax x => NonEmptyArray (Tuple Ident (Maybe x)) -> Dodo.Doc a
printLet bindings = do
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
                esAssign ident (snd (printSyntax (syntaxOf b)))
        )
        bindings
    ]

printConst :: forall x a. HasSyntax x => NonEmptyArray (Tuple Ident x) -> Dodo.Doc a
printConst bindings = do
  let kw = Dodo.text "const"
  let sep = Dodo.flexAlt (Dodo.text ", ") (Dodo.text ";" <> Dodo.break <> kw <> Dodo.space)
  Dodo.flexGroup $ Dodo.words
    [ kw
    , Dodo.foldWithSeparator sep $ map
        ( \(Tuple ident b) ->
            esAssign ident (snd (printSyntax (syntaxOf b)))
        )
        bindings
    ]

printIfElse :: forall x a. HasSyntax x => x -> Array x -> Array x -> Dodo.Doc a
printIfElse cond as bs = Dodo.lines
  [ Dodo.flexGroup $ Dodo.words
      [ Dodo.text "if"
      , Dodo.Common.jsParens $ snd $ printSyntax $ syntaxOf cond
      , fold
          [ Dodo.text "{"
          , Dodo.spaceBreak
          , Dodo.indent $ Dodo.lines $ printStatement <$> as
          , Dodo.spaceBreak
          , Dodo.text "}"
          ]
      ]
  , if Array.null bs then
      mempty
    else
      Dodo.flexGroup $ fold
        [ Dodo.text "else"
        , Dodo.spaceBreak
        , Dodo.indent $ Dodo.lines $ printStatement <$> bs
        , Dodo.spaceBreak
        , Dodo.text "}"
        ]
  ]

printWhile :: forall x a. HasSyntax x => x -> Array x -> Dodo.Doc a
printWhile cond as = Dodo.flexGroup $ Dodo.words
  [ Dodo.text "while"
  , Dodo.Common.jsParens $ snd $ printSyntax $ syntaxOf cond
  , fold
      [ Dodo.text "{"
      , Dodo.spaceBreak
      , Dodo.indent $ Dodo.lines $ printStatement <$> as
      , Dodo.spaceBreak
      , Dodo.text "}"
      ]
  ]
