module PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax
  ( EsSyntax(..)
  , EsAssignment(..)
  , EsArrayElement(..)
  , EsObjectElement(..)
  , EsBinaryOp(..)
  , EsUnaryOp(..)
  , EsRuntimeOp(..)
  , EsPrec(..)
  , EsAnalysis(..)
  , EsExpr(..)
  , EsIdent(..)
  , esAnalysisOf
  , build
  , print
  , class HasSyntax
  , syntaxOf
  , esArrowFunction
  , esCurriedFunction
  , esBinding
  , fromIdent
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (class Foldable, fold, foldMap, foldlDefault, foldr, foldrDefault)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Common (esAccessor, esApp, esAssign, esBoolean, esEscapeIdent, esEscapeProp, esIdent, esIndex, esInt, esModuleName, esNumber, esString)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), ModuleName, Qualified(..))

data EsIdent
  = Embedded Ident String
  | Generated String

data EsSyntax a
  = EsString String
  | EsNumber Number
  | EsInt Int
  | EsBoolean Boolean
  | EsArray (Array (EsArrayElement a))
  | EsObject (Array (EsObjectElement a))
  | EsAccess a String
  | EsIndex a Int
  | EsIdent (Qualified EsIdent)
  | EsRuntime (EsRuntimeOp a)
  | EsCall a (Array a)
  | EsBinary EsBinaryOp a a
  | EsUnary EsUnaryOp a
  | EsAssign EsAssignment a
  | EsArrowFunction (Array Ident) (Array a)
  | EsCommentTrailing a String
  | EsConst (NonEmptyArray (Tuple Ident a))
  | EsLet (NonEmptyArray (Tuple Ident (Maybe a)))
  | EsIfElse a (Array a) (Array a)
  | EsWhile a (Array a)
  | EsReturn a
  | EsContinue
  | EsUndefined

derive instance Functor EsSyntax

instance Foldable EsSyntax where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    EsString _ -> mempty
    EsNumber _ -> mempty
    EsInt _ -> mempty
    EsBoolean _ -> mempty
    EsArray as -> foldMap (foldMap f) as
    EsObject as -> foldMap (foldMap f) as
    EsAccess a _ -> f a
    EsIndex a _ -> f a
    EsIdent _ -> mempty
    EsRuntime a -> foldMap f a
    EsCall a bs -> f a <> foldMap f bs
    EsBinary _ a b -> f a <> f b
    EsUnary _ a -> f a
    EsAssign _ a -> f a
    EsArrowFunction _ as -> foldMap f as
    EsCommentTrailing a _ -> f a
    EsConst as -> foldMap (foldMap f) as
    EsLet as -> foldMap (foldMap (foldMap f)) as
    EsIfElse a bs cs -> f a <> foldMap f bs <> foldMap f cs
    EsWhile a bs -> f a <> foldMap f bs
    EsReturn a -> f a
    EsContinue -> mempty
    EsUndefined -> mempty

data EsAssignment
  = EsAssignIdent Ident
  | EsAssignAccess EsAssignment String

data EsArrayElement a
  = EsArrayValue a
  | EsArraySpread a

derive instance Functor EsArrayElement

instance Foldable EsArrayElement where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    EsArrayValue a -> f a
    EsArraySpread a -> f a

data EsObjectElement a
  = EsObjectPun String
  | EsObjectField String a
  | EsObjectSpread a

derive instance Functor EsObjectElement

instance Foldable EsObjectElement where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    EsObjectPun _ -> mempty
    EsObjectField _ a -> f a
    EsObjectSpread a -> f a

data EsBinaryOp
  = EsOr
  | EsAnd
  | EsLessThan
  | EsLessThanEqual
  | EsGreaterThan
  | EsGreaterThanEqual
  | EsAdd
  | EsSubtract
  | EsDivide
  | EsMultiply
  | EsBitAnd
  | EsBitOr
  | EsBitShiftLeft
  | EsBitShitRight
  | EsBitXor
  | EsZeroFillShiftRight
  | EsEquals
  | EsNotEquals

data EsUnaryOp
  = EsNot
  | EsNegate
  | EsBitNegate

data EsPrec
  = EsPrecStatement
  | EsPrecControl
  | EsPrecArrow
  | EsPrecAssign
  | EsPrecBinary Int
  | EsPrecPrefix
  | EsPrecCall
  | EsPrecAtom

derive instance Eq EsPrec
derive instance Ord EsPrec

data EsRuntimeOp a
  = EsBinding a
  | EsFail

derive instance Functor EsRuntimeOp

instance Foldable EsRuntimeOp where
  foldMap f = case _ of
    EsBinding a -> f a
    EsFail -> mempty
  foldr a = foldrDefault a
  foldl a = foldlDefault a

data EsExpr = EsExpr EsAnalysis (EsSyntax EsExpr)

newtype EsAnalysis = EsAnalysis
  { deps :: Set ModuleName
  , runtime :: Boolean
  , pure :: Boolean
  }

instance Semigroup EsAnalysis where
  append (EsAnalysis a) (EsAnalysis b) = EsAnalysis
    { deps: a.deps <> b.deps
    , runtime: a.runtime || b.runtime
    , pure: a.pure && b.pure
    }

instance Monoid EsAnalysis where
  mempty = EsAnalysis { deps: mempty, runtime: false, pure: true }

needsDep :: ModuleName -> EsAnalysis -> EsAnalysis
needsDep mn (EsAnalysis a) = EsAnalysis a { deps = Set.insert mn a.deps }

needsRuntime :: EsAnalysis -> EsAnalysis
needsRuntime (EsAnalysis a) = EsAnalysis a { runtime = true }

notPure :: EsAnalysis -> EsAnalysis
notPure (EsAnalysis a) = EsAnalysis a { pure = false }

esAnalysisOf :: EsExpr -> EsAnalysis
esAnalysisOf (EsExpr a _) = a

build :: EsSyntax EsExpr -> EsExpr
build syn = case syn of
  EsIdent (Qualified (Just mn) _) ->
    EsExpr (needsDep mn mempty) syn
  EsRuntime op ->
    EsExpr (needsRuntime (foldMap esAnalysisOf op)) syn
  _ ->
    EsExpr (pureAnn (foldMap esAnalysisOf syn)) syn
    where
    pureAnn = case syn of
      EsAccess _ _ -> notPure
      EsIndex _ _ -> notPure
      EsBinary _ _ _ -> notPure
      EsUnary _ _ -> notPure
      EsAssign _ _ -> notPure
      EsArrowFunction _ _ -> notPure
      _ -> identity

class HasSyntax a where
  syntaxOf :: a -> EsSyntax a

wrapPrec :: forall a. EsPrec -> Tuple EsPrec (Dodo.Doc a) -> Dodo.Doc a
wrapPrec p1 (Tuple p2 doc)
  | p2 > p1 = Dodo.Common.jsParens doc
  | otherwise = doc

print :: forall x a. HasSyntax x => EsSyntax x -> Tuple EsPrec (Dodo.Doc a)
print syn = case syn of
  EsString str ->
    Tuple EsPrecAtom $ esString str
  EsNumber num ->
    Tuple EsPrecAtom $ esNumber num
  EsInt int ->
    Tuple EsPrecAtom $ esInt int
  EsBoolean bool ->
    Tuple EsPrecAtom $ esBoolean bool
  EsArray as ->
    Tuple EsPrecAtom $ printArray as
  EsObject as ->
    Tuple EsPrecAtom $ printObject as
  EsIdent (Qualified mb ident) ->
    case mb of
      Nothing ->
        Tuple EsPrecAtom $ printIdent ident
      Just mn ->
        Tuple EsPrecCall $ esModuleName mn <> Dodo.text "." <> printIdent ident
  EsAccess a prop -> do
    let p1 = EsPrecCall
    let a' = wrapPrec p1 (print (syntaxOf a))
    Tuple p1 $ esAccessor a' prop
  EsIndex a ix -> do
    let p1 = EsPrecCall
    let a' = wrapPrec p1 (print (syntaxOf a))
    Tuple p1 $ esIndex a' ix
  EsRuntime op ->
    Tuple EsPrecCall $ case op of
      EsBinding a ->
        esApp (Dodo.text "$runtime.binding") [ snd (print (syntaxOf a)) ]
      EsFail ->
        Dodo.text "$runtime.fail()"
  EsCall a bs -> do
    let p1 = EsPrecCall
    let a' = wrapPrec p1 (print (syntaxOf a))
    Tuple EsPrecCall $ esApp a' (snd <<< print <<< syntaxOf <$> bs)
  EsBinary op a b -> do
    let Tuple pn str = printEsBinaryOp op
    let p1 = EsPrecBinary pn
    let a' = print (syntaxOf a)
    let b' = print (syntaxOf b)
    Tuple p1 $ Dodo.words [ wrapPrec p1 a', Dodo.text str, wrapPrec p1 b' ]
  EsUnary op a -> do
    let p1 = EsPrecPrefix
    let a' = wrapPrec p1 (print (syntaxOf a))
    Tuple p1 $  Dodo.words [ Dodo.text (printEsUnaryOp op), a' ]
  EsAssign asn b ->
    Tuple EsPrecStatement $ Dodo.words [ printAssignment asn, Dodo.text "=", snd (print (syntaxOf b)) ]
  EsArrowFunction args a ->
    Tuple EsPrecArrow $ printArrowFunction args a
  EsCommentTrailing a comment -> do
    let Tuple p doc = print (syntaxOf a)
    Tuple p $ Dodo.words [ doc, Dodo.text "/*", Dodo.text comment, Dodo.text "*/" ]
  EsConst bindings ->
    Tuple EsPrecStatement $ printConst bindings
  EsLet bindings ->
    Tuple EsPrecStatement $ printLet bindings
  EsIfElse a bs cs ->
    Tuple EsPrecControl $ printIfElse a bs cs
  EsWhile a bs ->
    Tuple EsPrecControl $ printWhile a bs
  EsReturn a ->
    Tuple EsPrecStatement $ Dodo.words [ Dodo.text "return", snd (print (syntaxOf a)) ]
  EsContinue ->
    Tuple EsPrecStatement $ Dodo.text "continue"
  EsUndefined ->
    Tuple EsPrecAtom $ Dodo.text "undefined"

printEsBinaryOp :: EsBinaryOp -> Tuple Int String
printEsBinaryOp = case _ of
  EsOr -> Tuple 1 "||"
  EsAnd -> Tuple 2 "&&"
  EsBitOr -> Tuple 3 "|"
  EsBitXor -> Tuple 4 "^"
  EsBitAnd -> Tuple 5 "&"
  EsEquals -> Tuple 6 "==="
  EsNotEquals -> Tuple 6 "!=="
  EsLessThan -> Tuple 7 "<"
  EsLessThanEqual -> Tuple 7 "<="
  EsGreaterThan -> Tuple 7 ">"
  EsGreaterThanEqual -> Tuple 7 ">="
  EsBitShiftLeft -> Tuple 8 "<<"
  EsBitShitRight -> Tuple 8 ">>"
  EsZeroFillShiftRight -> Tuple 8 ">>>"
  EsAdd -> Tuple 9 "+"
  EsSubtract -> Tuple 9 "-"
  EsDivide -> Tuple 10 "/"
  EsMultiply -> Tuple 10 "*"

printEsUnaryOp :: EsUnaryOp -> String
printEsUnaryOp = case _ of
  EsNot -> "!"
  EsNegate -> "-"
  EsBitNegate -> "~"

printArrayElement :: forall x a. HasSyntax x => EsArrayElement x -> Dodo.Doc a
printArrayElement = case _ of
  EsArrayValue a ->
    snd (print (syntaxOf a))
  EsArraySpread a ->
    snd (print (syntaxOf a)) <> Dodo.text "..."

printObjectElement :: forall x a. HasSyntax x => EsObjectElement x -> Dodo.Doc a
printObjectElement = case _ of
  EsObjectPun field ->
    Dodo.text (fromMaybe field (esEscapeProp field))
  EsObjectField field a ->
    Dodo.text (fromMaybe field (esEscapeProp field))
      <> Dodo.text ":"
      <> Dodo.space
      <> snd (print (syntaxOf a))
  EsObjectSpread a ->
    snd (print (syntaxOf a)) <> Dodo.text "..."

printArray :: forall x a. HasSyntax x => Array (EsArrayElement x) -> Dodo.Doc a
printArray =
  Dodo.Common.jsSquares
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map printArrayElement

printObject :: forall x a. HasSyntax x => Array (EsObjectElement x) -> Dodo.Doc a
printObject =
  Dodo.Common.jsCurlies
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map printObjectElement

printAssignment :: forall a. EsAssignment -> Dodo.Doc a
printAssignment = case _ of
  EsAssignIdent ident ->
    esIdent ident
  EsAssignAccess a prop ->
    esAccessor (printAssignment a) prop

printArrowFunction :: forall x a. HasSyntax x => Array Ident -> Array x -> Dodo.Doc a
printArrowFunction args stmts = Dodo.words
  [ if Array.length args == 1 then
      foldMap esIdent args
    else
      Dodo.Common.jsParens $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ esIdent <$> args
  , Dodo.text "=>"
  , case stmts of
      [ stmt ] | EsReturn a <- syntaxOf stmt ->
        case syntaxOf a of
          EsObject as ->
            Dodo.Common.jsParens $ printObject as
          other ->
            snd $ print other
      _ ->
        Dodo.Common.jsCurlies $ Dodo.lines $ printStatement <$> stmts
  ]

printStatement :: forall x a. HasSyntax x => x -> Dodo.Doc a
printStatement x = do
  let Tuple prec doc = print (syntaxOf x)
  case prec of
    EsPrecControl ->
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
                esAssign ident (snd (print (syntaxOf b)))
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
            esAssign ident (snd (print (syntaxOf b)))
        )
        bindings
    ]

printIfElse :: forall x a. HasSyntax x => x -> Array x -> Array x -> Dodo.Doc a
printIfElse cond as bs = Dodo.lines
  [ Dodo.flexGroup $ Dodo.words
      [ Dodo.text "if"
      , Dodo.Common.jsParens $ snd $ print $ syntaxOf cond
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
  , Dodo.Common.jsParens $ snd $ print $ syntaxOf cond
  , fold
      [ Dodo.text "{"
      , Dodo.spaceBreak
      , Dodo.indent $ Dodo.lines $ printStatement <$> as
      , Dodo.spaceBreak
      , Dodo.text "}"
      ]
  ]

printIdent :: forall a. EsIdent -> Dodo.Doc a
printIdent = Dodo.text <<< printIdentString

printIdentString :: EsIdent -> String
printIdentString = case _ of
  Embedded (Ident id) "" ->
    esEscapeIdent id
  Embedded (Ident id) suff ->
    esEscapeIdent id <> "$" <> suff
  Generated id ->
    id

fromIdent :: Ident -> EsIdent
fromIdent = flip Embedded ""

esArrowFunction :: Array Ident -> Array EsExpr -> EsExpr
esArrowFunction args = build <<< EsArrowFunction args

esCurriedFunction :: Array Ident -> Array EsExpr -> EsExpr
esCurriedFunction args stmts = case Array.unsnoc args of
  Nothing ->
    esArrowFunction [] stmts
  Just { init, last } ->
    foldr (\a -> build <<< EsArrowFunction [ a ] <<< pure) (build (EsArrowFunction [ last ] stmts)) init

esBinding :: Ident -> EsExpr -> EsExpr
esBinding ident expr = build $ EsConst $ NonEmptyArray.singleton $ Tuple ident expr
