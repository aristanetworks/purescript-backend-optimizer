-- @inline export wrapPrecWith arity=1
module PureScript.Backend.Optimizer.Codegen.EcmaScript.Syntax
  ( EsModuleStatement(..)
  , EsSyntax(..)
  , EsArrayElement(..)
  , EsObjectElement(..)
  , EsBindingPattern(..)
  , EsBinaryOp(..)
  , EsUnaryOp(..)
  , EsRuntimeOp(..)
  , EsPrec(..)
  , EsAnalysis(..)
  , EsExpr(..)
  , EsIdent(..)
  , esAnalysisOf
  , build
  , PrintOptions
  , defaultPrintOptions
  , print
  , printModuleStatement
  , printStatement
  , printIdentString
  , class HasSyntax
  , syntaxOf
  , esArrowFunction
  , esCurriedFunction
  , esBinding
  , esLazyBinding
  , esAssignIdent
  , class ToEsIdent
  , toEsIdent
  , toEsIdentWith
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldlDefault, foldr, foldrDefault)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Optimizer.Codegen.EcmaScript.Common (esAccessor, esApp, esAssign, esBoolean, esEscapeIdent, esEscapeProp, esEscapeSpecial, esIndex, esInt, esModuleName, esNumber, esString, esTernary)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), ModuleName(..), Qualified(..))

data EsModuleStatement a
  = EsImport (Array EsIdent) String
  | EsImportAllAs EsIdent String
  | EsExport (Array EsIdent) (Maybe String)
  | EsExportAllFrom String
  | EsStatement a

data EsIdent
  = Embedded Ident String
  | Generated String

derive instance Eq EsIdent
derive instance Ord EsIdent

data EsSyntax a
  = EsString String
  | EsNumber Number
  | EsInt Int
  | EsBoolean Boolean
  | EsArray (Array (EsArrayElement a))
  | EsObject (Array (EsObjectElement a))
  | EsAccess a String
  | EsIndex a a
  | EsIdent (Qualified EsIdent)
  | EsRuntime (EsRuntimeOp a)
  | EsCall a (Array (EsArrayElement a))
  | EsTernary a a a
  | EsBinary EsBinaryOp a a
  | EsUnary EsUnaryOp a
  | EsAssign a a
  | EsArrowFunction (Array EsIdent) (Array a)
  | EsCommentTrailing a String
  | EsConst (NonEmptyArray (Tuple EsBindingPattern a))
  | EsLet (NonEmptyArray (Tuple EsIdent (Maybe a)))
  | EsIfElse a (Array a) (Array a)
  | EsWhile a (Array a)
  | EsForOf EsBindingPattern a (Array a)
  | EsReturn (Maybe a)
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
    EsIndex a b -> f a <> f b
    EsIdent _ -> mempty
    EsRuntime a -> foldMap f a
    EsCall a bs -> f a <> foldMap (foldMap f) bs
    EsTernary a b c -> f a <> f b <> f c
    EsBinary _ a b -> f a <> f b
    EsUnary _ a -> f a
    EsAssign _ a -> f a
    EsArrowFunction _ as -> foldMap f as
    EsCommentTrailing a _ -> f a
    EsConst as -> foldMap (foldMap f) as
    EsLet as -> foldMap (foldMap (foldMap f)) as
    EsIfElse a bs cs -> f a <> foldMap f bs <> foldMap f cs
    EsWhile a bs -> f a <> foldMap f bs
    EsForOf _ b cs -> f b <> foldMap f cs
    EsReturn a -> foldMap f a
    EsContinue -> mempty
    EsUndefined -> mempty

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
  = EsObjectPun EsIdent
  | EsObjectField String a
  | EsObjectSpread a

derive instance Functor EsObjectElement

data EsBindingPattern = EsBindingIdent EsIdent

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
  | EsDelete

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
  | EsRange a a
  | EsFail

derive instance Functor EsRuntimeOp

instance Foldable EsRuntimeOp where
  foldMap f = case _ of
    EsBinding a -> f a
    EsRange a b -> f a <> f b
    EsFail -> mempty
  foldr a = foldrDefault a
  foldl a = foldlDefault a

data EsExpr = EsExpr EsAnalysis (EsSyntax EsExpr)

instance HasSyntax EsExpr where
  syntaxOf (EsExpr _ syn) = syn

newtype EsAnalysis = EsAnalysis
  { deps :: Set ModuleName
  , runtime :: Boolean
  , pure :: Boolean
  }

derive instance Newtype EsAnalysis _

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

alwaysPure :: EsAnalysis -> EsAnalysis
alwaysPure (EsAnalysis a) = EsAnalysis a { pure = true }

esAnalysisOf :: EsExpr -> EsAnalysis
esAnalysisOf (EsExpr a _) = a

build :: EsSyntax EsExpr -> EsExpr
build syn = case syn of
  EsIdent (Qualified (Just mn) _) ->
    EsExpr (needsDep mn mempty) syn
  EsRuntime op ->
    EsExpr (needsRuntime (foldMap esAnalysisOf op)) syn
  EsCall (EsExpr _ (EsArrowFunction [] bs)) []
    | Just expr <- inlineCallBlock bs ->
        expr
  EsReturn (Just b)
    | Just expr' <- inlineLoopBlockStatement b ->
        expr'
  EsReturn (Just (EsExpr _ EsUndefined)) ->
    build $ EsReturn Nothing
  EsArrowFunction as bs
    | Just (EsExpr _ (EsReturn Nothing)) <- Array.last bs ->
        build $ EsArrowFunction as $ Array.dropEnd 1 bs
  EsArrowFunction as [ block ]
    | Just bs <- inlineReturnBlock block ->
        build $ EsArrowFunction as bs
  EsArrowFunction as bs -> do
    let Tuple s bs' = buildStatements bs
    EsExpr (alwaysPure s) $ EsArrowFunction as bs'
  EsIfElse a [ block ] cs
    | Just bs <- inlineReturnBlock block ->
        build $ EsIfElse a bs cs
  EsIfElse a bs [ block ]
    | Just cs <- inlineReturnBlock block ->
        build $ EsIfElse a bs cs
  EsIfElse a bs cs -> do
    let Tuple s1 bs' = buildStatements bs
    let Tuple s2 cs' = buildStatements cs
    EsExpr (esAnalysisOf a <> s1 <> s2) $ EsIfElse a bs' cs'
  EsWhile a bs
    | Just bs' <- removeTrailingContinue bs ->
        build $ EsWhile a bs'
    | otherwise -> do
        let Tuple s bs' = buildStatements bs
        EsExpr (esAnalysisOf a <> s) $ EsWhile a bs'
  EsForOf a b cs
    | Just cs' <- removeTrailingContinue cs ->
        build $ EsForOf a b cs'
    | otherwise -> do
        let Tuple s cs' = buildStatements cs
        EsExpr (esAnalysisOf b <> s) $ EsForOf a b cs'
  _ ->
    EsExpr (pureAnn (foldMap esAnalysisOf syn)) syn
    where
    pureAnn = case syn of
      EsAccess _ _ -> notPure
      EsIndex _ _ -> notPure
      EsBinary _ _ _ -> notPure
      EsUnary _ _ -> notPure
      EsAssign _ _ -> notPure
      EsArrowFunction _ _ -> alwaysPure
      _ -> identity

buildStatements :: Array EsExpr -> Tuple EsAnalysis (Array EsExpr)
buildStatements = traverse go
  where
  go expr = case expr of
    _ | Just expr' <- inlineLoopBlockStatement expr ->
      go expr'
    _ ->
      Tuple (esAnalysisOf expr) expr

inlineReturnBlock :: EsExpr -> Maybe (Array EsExpr)
inlineReturnBlock (EsExpr _ expr) = case expr of
  EsReturn (Just (EsExpr _ (EsCall (EsExpr _ (EsArrowFunction [] bs)) []))) ->
    Just bs
  _ ->
    Nothing

inlineCallBlock :: Array EsExpr -> Maybe EsExpr
inlineCallBlock = case _ of
  [ EsExpr _ (EsReturn (Just expr)) ] ->
    Just expr
  [ EsExpr _ (EsIfElse a [ EsExpr _ (EsReturn (Just b)) ] []), EsExpr _ (EsReturn (Just c)) ] ->
    Just $ build $ EsTernary a b c
  _ ->
    Nothing

inlineLoopBlockStatement :: EsExpr -> Maybe EsExpr
inlineLoopBlockStatement (EsExpr _ expr) = case expr of
  EsCall (EsExpr _ (EsArrowFunction [] [ b@(EsExpr _ loop) ])) []
    | isLoop loop ->
        Just b
  _ ->
    Nothing

removeTrailingContinue :: Array EsExpr -> Maybe (Array EsExpr)
removeTrailingContinue stmts = case Array.last stmts of
  Just (EsExpr s (EsIfElse a bs []))
    | Just cs <- removeTrailingContinue bs ->
        Just $ Array.snoc (Array.dropEnd 1 stmts) $ EsExpr s $ EsIfElse a cs []
  Just (EsExpr _ EsContinue) ->
    Just (Array.dropEnd 1 stmts)
  _ ->
    Nothing

isLoop :: forall a. EsSyntax a -> Boolean
isLoop = case _ of
  EsForOf _ _ _ -> true
  EsWhile _ _ -> true
  _ -> false

class HasSyntax a where
  syntaxOf :: a -> EsSyntax a

wrapPrec :: forall a. EsPrec -> Tuple EsPrec (Dodo.Doc a) -> Dodo.Doc a
wrapPrec = wrapPrecWith (>)

wrapPrecGte :: forall a. EsPrec -> Tuple EsPrec (Dodo.Doc a) -> Dodo.Doc a
wrapPrecGte = wrapPrecWith (>=)

wrapPrecWith
  :: forall a
   . (EsPrec -> EsPrec -> Boolean)
  -> EsPrec
  -> Tuple EsPrec (Dodo.Doc a)
  -> Dodo.Doc a
wrapPrecWith f p1 (Tuple p2 doc)
  | f p1 p2 =
      case p2 of
        EsPrecArrow ->
          Dodo.text "(" <> doc <> Dodo.text ")"
        _ ->
          Dodo.Common.jsParens doc
  | otherwise = doc

type PrintOptions =
  { pureAnns :: Boolean
  }

defaultPrintOptions :: PrintOptions
defaultPrintOptions =
  { pureAnns: true
  }

print :: forall a. PrintOptions -> EsSyntax EsExpr -> Tuple EsPrec (Dodo.Doc a)
print opts syn = case syn of
  EsString str ->
    Tuple EsPrecAtom $ esString str
  EsNumber num ->
    Tuple EsPrecAtom $ esNumber num
  EsInt int ->
    Tuple EsPrecAtom $ esInt int
  EsBoolean bool ->
    Tuple EsPrecAtom $ esBoolean bool
  EsArray as ->
    Tuple EsPrecAtom $ printArray opts as
  EsObject as ->
    Tuple EsPrecAtom $ printObject opts as
  EsIdent (Qualified mb ident) ->
    case mb of
      Nothing ->
        Tuple EsPrecAtom $ printIdent ident
      Just mn ->
        Tuple EsPrecCall $ esModuleName mn <> Dodo.text "." <> printIdentQualified ident
  EsAccess a prop -> do
    let p1 = EsPrecCall
    let a' = wrapPrec p1 (print opts (syntaxOf a))
    Tuple p1 $ esAccessor a' prop
  EsIndex a ix -> do
    let p1 = EsPrecCall
    let a' = wrapPrec p1 (print opts (syntaxOf a))
    let ix' = snd (print opts (syntaxOf ix))
    Tuple p1 $ esIndex a' ix'
  EsRuntime op ->
    Tuple EsPrecCall $ case op of
      EsBinding a ->
        printPure opts $ esApp (Dodo.text "$runtime.binding") [ snd (print opts (syntaxOf a)) ]
      EsRange a b ->
        esApp (Dodo.text "$runtime.range")
          [ snd (print opts (syntaxOf a))
          , snd (print opts (syntaxOf b))
          ]
      EsFail ->
        Dodo.text "$runtime.fail()"
  EsCall a bs -> do
    let p1 = EsPrecCall
    let as = syntaxOf a
    let a' = wrapPrec p1 (print opts as)
    let doc = esApp a' (printArrayElement opts <$> bs)
    Tuple EsPrecCall $ case as of
      EsCall _ _ -> doc
      _ -> printPure opts doc
  EsTernary a b c -> do
    let p1 = EsPrecArrow
    let a' = print opts (syntaxOf a)
    let b' = print opts (syntaxOf b)
    let c' = print opts (syntaxOf c)
    Tuple p1 $ esTernary (wrapPrecGte p1 a') (wrapPrecGte p1 b') (wrapPrec p1 c')
  EsBinary op lhs rhs -> do
    printEsBinaryOp opts (esBinaryFixity op) lhs rhs
  EsUnary op a -> do
    let p1 = EsPrecPrefix
    let a' = wrapPrec p1 (print opts (syntaxOf a))
    Tuple p1 $ Dodo.text (printEsUnaryOp op) <> a'
  EsAssign a b ->
    Tuple EsPrecStatement $ Dodo.words [ snd (print opts (syntaxOf a)), Dodo.text "=", snd (print opts (syntaxOf b)) ]
  EsArrowFunction args a ->
    Tuple EsPrecArrow $ printArrowFunction (noPureAnns opts) args a
  EsCommentTrailing a comment -> do
    let Tuple p doc = print opts (syntaxOf a)
    Tuple p $ Dodo.words [ doc, Dodo.text "/*", Dodo.text comment, Dodo.text "*/" ]
  EsConst bindings ->
    Tuple EsPrecStatement $ printConst opts bindings
  EsLet bindings ->
    Tuple EsPrecStatement $ printLet opts bindings
  EsIfElse a bs cs ->
    Tuple EsPrecControl $ printIfElse opts a bs cs
  EsWhile a bs ->
    Tuple EsPrecControl $ printWhile opts a bs
  EsForOf a b cs ->
    Tuple EsPrecControl $ printForOf opts a b cs
  EsReturn (Just a) ->
    Tuple EsPrecStatement $ Dodo.words [ Dodo.text "return", snd (print opts (syntaxOf a)) ]
  EsReturn Nothing ->
    Tuple EsPrecStatement $ Dodo.text "return"
  EsContinue ->
    Tuple EsPrecStatement $ Dodo.text "continue"
  EsUndefined ->
    Tuple EsPrecAtom $ Dodo.text "undefined"

printEsBinaryOp :: forall a. PrintOptions -> EsBinaryFixity -> EsExpr -> EsExpr -> Tuple EsPrec (Dodo.Doc a)
printEsBinaryOp opts f1 (EsExpr _ lhs) (EsExpr _ rhs) =
  Tuple p1 $ Dodo.words [ lhsDoc, Dodo.text f1.symbol, rhsDoc ]
  where
  p1 :: EsPrec
  p1 = EsPrecBinary f1.precedence

  lhsDoc :: Dodo.Doc a
  lhsDoc = case lhs of
    EsBinary op2 lhs' rhs' -> do
      let f2 = esBinaryFixity op2
      let doc = snd $ printEsBinaryOp opts f2 lhs' rhs'
      if f2.precedence >= f1.precedence then
        doc
      else
        Dodo.Common.jsParens doc
    _ ->
      wrapPrecGte p1 (print opts lhs)

  rhsDoc :: Dodo.Doc a
  rhsDoc = case rhs of
    EsBinary binOp2 lhs' rhs' -> do
      let f2 = esBinaryFixity binOp2
      let doc = snd $ printEsBinaryOp opts f2 lhs' rhs'
      if (f1.symbol == f2.symbol && f1.associative) || f2.precedence > f1.precedence then
        doc
      else
        Dodo.Common.jsParens doc
    _ ->
      wrapPrecGte p1 (print opts rhs)

type EsBinaryFixity  =
  { associative :: Boolean
  , precedence :: Int
  , symbol :: String
  }

esBinaryFixity :: EsBinaryOp -> EsBinaryFixity
esBinaryFixity= case _ of
  EsOr -> fixity true 1 "||"
  EsAnd -> fixity true 2 "&&"
  EsBitOr -> fixity true 3 "|"
  EsBitXor -> fixity true 4 "^"
  EsBitAnd -> fixity true 5 "&"
  EsEquals -> fixity false 6 "==="
  EsNotEquals -> fixity false 6 "!=="
  EsLessThan -> fixity false 7 "<"
  EsLessThanEqual -> fixity false 7 "<="
  EsGreaterThan -> fixity false 7 ">"
  EsGreaterThanEqual -> fixity false 7 ">="
  EsBitShiftLeft -> fixity false 8 "<<"
  EsBitShitRight -> fixity false 8 ">>"
  EsZeroFillShiftRight -> fixity false 8 ">>>"
  EsAdd -> fixity true 9 "+"
  EsSubtract -> fixity true 9 "-"
  EsDivide -> fixity true 10 "/"
  EsMultiply -> fixity true 10 "*"
  where
  fixity associative precedence symbol =
    { associative, precedence, symbol }

printEsUnaryOp :: EsUnaryOp -> String
printEsUnaryOp = case _ of
  EsNot -> "!"
  EsNegate -> "-"
  EsBitNegate -> "~"
  EsDelete -> "delete "

printArrayElement :: forall a. PrintOptions -> EsArrayElement EsExpr -> Dodo.Doc a
printArrayElement opts = case _ of
  EsArrayValue a ->
    snd (print opts (syntaxOf a))
  EsArraySpread a ->
    Dodo.text "..." <> snd (print opts (syntaxOf a))

printObjectElement :: forall a. PrintOptions -> EsObjectElement EsExpr -> Dodo.Doc a
printObjectElement opts = case _ of
  EsObjectPun field ->
    printIdent field
  EsObjectField field a ->
    Dodo.text (fromMaybe field (esEscapeProp field))
      <> Dodo.text ":"
      <> Dodo.space
      <> snd (print opts (syntaxOf a))
  EsObjectSpread a ->
    Dodo.text "..." <> snd (print opts (syntaxOf a))

printArray :: forall a. PrintOptions -> Array (EsArrayElement EsExpr) -> Dodo.Doc a
printArray opts =
  Dodo.Common.jsSquares
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map (printArrayElement opts)

printObject :: forall a. PrintOptions -> Array (EsObjectElement EsExpr) -> Dodo.Doc a
printObject opts =
  Dodo.Common.jsCurlies
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map (printObjectElement opts)

printArrowFunction :: forall a. PrintOptions -> Array EsIdent -> Array EsExpr -> Dodo.Doc a
printArrowFunction opts args stmts = Dodo.words
  [ case args of
      [ arg ] -> do
        let str = printIdentString arg
        if isJust $ String.stripPrefix (String.Pattern "$__unused") str then
          Dodo.text "()"
        else
          Dodo.text str
      _ ->
        Dodo.Common.jsParens $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ printIdent <$> args
  , Dodo.text "=>"
  , case stmts of
      [ stmt ] | EsReturn (Just a) <- syntaxOf stmt ->
        case syntaxOf a of
          EsObject as ->
            Dodo.Common.jsParens $ printObject opts as
          other ->
            snd $ print opts other
      _ ->
        Dodo.Common.jsCurlies $ Dodo.lines $ printStatement opts <$> stmts
  ]

printStatement :: forall a. PrintOptions -> EsExpr -> Dodo.Doc a
printStatement opts x = do
  let Tuple prec doc = print opts (syntaxOf x)
  case prec of
    EsPrecControl ->
      doc
    _ ->
      doc <> Dodo.text ";"

printLet :: forall a. PrintOptions -> NonEmptyArray (Tuple EsIdent (Maybe EsExpr)) -> Dodo.Doc a
printLet opts bindings = do
  let kw = Dodo.text "let"
  let sep = Dodo.flexAlt (Dodo.text ", ") (Dodo.text ";" <> Dodo.break <> kw <> Dodo.space)
  Dodo.flexGroup $ Dodo.words
    [ kw
    , Dodo.foldWithSeparator sep $ map
        ( \(Tuple ident mb) ->
            case mb of
              Nothing ->
                printIdent ident
              Just b ->
                esAssign (printIdent ident) (printBindingValue opts b)
        )
        bindings
    ]

printConst :: forall a. PrintOptions -> NonEmptyArray (Tuple EsBindingPattern EsExpr) -> Dodo.Doc a
printConst opts bindings = do
  let kw = Dodo.text "const"
  let sep = Dodo.flexAlt (Dodo.text ", ") (Dodo.text ";" <> Dodo.break <> kw <> Dodo.space)
  Dodo.flexGroup $ Dodo.words
    [ kw
    , Dodo.foldWithSeparator sep $ map
        ( \(Tuple ident b) ->
            esAssign (printBindingPattern ident) (printBindingValue opts b)
        )
        bindings
    ]

printBindingPattern :: forall a. EsBindingPattern -> Dodo.Doc a
printBindingPattern = case _ of
  EsBindingIdent ident ->
    printIdent ident

printBindingValue :: forall a. PrintOptions -> EsExpr -> Dodo.Doc a
printBindingValue opts val@(EsExpr (EsAnalysis s) _)
  | opts.pureAnns && not s.pure =
      snd $ print opts $ EsCall
        ( build $ EsArrowFunction []
            [ build $ EsReturn $ Just val ]
        )
        []
  | otherwise =
      snd $ print opts (syntaxOf val)

printIfElse :: forall a. PrintOptions -> EsExpr -> Array EsExpr -> Array EsExpr -> Dodo.Doc a
printIfElse opts cond as bs = do
  let Tuple last conds = toIfElseChain (List.singleton (Tuple cond as)) bs
  foldl
    ( \elseDoc (Tuple cond' as') -> do
        let
          ifDoc = Dodo.words
            [ Dodo.text "if"
            , Dodo.Common.jsParens $ snd $ print opts $ syntaxOf cond'
            , fold
                [ Dodo.text "{"
                , Dodo.spaceBreak
                , Dodo.indent $ Dodo.lines $ printStatement opts <$> as'
                , Dodo.spaceBreak
                , Dodo.text "}"
                ]
            ]
        if Dodo.isEmpty elseDoc then
          Dodo.flexGroup ifDoc
        else
          Dodo.words
            [ ifDoc
            , Dodo.text "else"
            , elseDoc
            ]
    )
    ( if Array.null last then
        mempty
      else
        fold
          [ Dodo.text "{"
          , Dodo.spaceBreak
          , Dodo.indent $ Dodo.lines $ printStatement opts <$> last
          , Dodo.spaceBreak
          , Dodo.text "}"
          ]
    )
    conds
  where
  toIfElseChain acc = case _ of
    [ EsExpr _ (EsIfElse cond' as' bs') ] ->
      toIfElseChain (List.Cons (Tuple cond' as') acc) bs'
    bs' ->
      Tuple bs' acc

printWhile :: forall a. PrintOptions -> EsExpr -> Array EsExpr -> Dodo.Doc a
printWhile opts cond as
  | Array.null as = Dodo.words
      [ Dodo.text "while"
      , Dodo.Common.jsParens $ snd $ print opts $ syntaxOf cond
      ]
  | otherwise = Dodo.lines
      [ Dodo.words
          [ Dodo.text "while"
          , Dodo.Common.jsParens $ snd $ print opts $ syntaxOf cond
          , Dodo.text "{"
          ]
      , Dodo.indent $ Dodo.lines $ printStatement opts <$> as
      , Dodo.text "}"
      ]

printForOf :: forall a. PrintOptions -> EsBindingPattern -> EsExpr -> Array EsExpr -> Dodo.Doc a
printForOf opts binder iter as = Dodo.lines
  [ Dodo.words
      [ Dodo.text "for"
      , Dodo.Common.jsParens $ Dodo.words
          [ Dodo.text "const"
          , printBindingPattern binder
          , Dodo.text "of"
          , snd $ print opts (syntaxOf iter)
          ]
      , Dodo.text "{"
      ]
  , Dodo.indent $ Dodo.lines $ printStatement opts <$> as
  , Dodo.text "}"
  ]

printIdent :: forall a. EsIdent -> Dodo.Doc a
printIdent = Dodo.text <<< printIdentString

printIdentQualified :: forall a. EsIdent -> Dodo.Doc a
printIdentQualified = Dodo.text <<< printIdentStringEscape esEscapeSpecial

printIdentString :: EsIdent -> String
printIdentString = printIdentStringEscape esEscapeIdent

printIdentStringEscape :: (String -> String) -> EsIdent -> String
printIdentStringEscape esc = case _ of
  Embedded (Ident id) "" ->
    esc id
  Embedded (Ident id) suff ->
    esc id <> "$" <> suff
  Generated id ->
    id

printModuleStatement :: forall a. PrintOptions -> EsModuleStatement EsExpr -> Dodo.Doc a
printModuleStatement opts = case _ of
  EsImport imports path ->
    Dodo.words
      [ Dodo.text "import"
      , Dodo.Common.jsCurlies $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ map snd $ Array.sortBy (comparing fst) $ printImportElement <$> imports
      , Dodo.text "from"
      , esString path
      ]
      <> Dodo.text ";"
  EsImportAllAs ident path ->
    Dodo.words
      [ Dodo.text "import"
      , Dodo.text "*"
      , Dodo.text "as"
      , printIdent ident
      , Dodo.text "from"
      , esString path
      ]
      <> Dodo.text ";"
  EsExport exports mbPath ->
    Dodo.words
      [ Dodo.text "export"
      , Dodo.Common.jsCurlies $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ map snd $ Array.sortBy (comparing fst) $ printExportElement <$> exports
      , foldMap (\p -> Dodo.words [ Dodo.text "from", esString p ]) mbPath
      ]
      <> Dodo.text ";"
  EsExportAllFrom path ->
    Dodo.words
      [ Dodo.text "export"
      , Dodo.text "*"
      , Dodo.text "from"
      , esString path
      ]
      <> Dodo.text ";"
  EsStatement expr ->
    printStatement opts expr

printImportElement :: forall a. EsIdent -> Tuple String (Dodo.Doc a)
printImportElement id = do
  let id1 = printIdentString id
  let id2 = printIdentStringEscape esEscapeSpecial id
  if id1 == id2 then Tuple id1 $ Dodo.text id1
  else Tuple id1 $ Dodo.words [ Dodo.text id2, Dodo.text "as", Dodo.text id1 ]

printExportElement :: forall a. EsIdent -> Tuple String (Dodo.Doc a)
printExportElement id = do
  let id1 = printIdentString id
  let id2 = printIdentStringEscape esEscapeSpecial id
  if id1 == id2 then Tuple id1 $ Dodo.text id1
  else Tuple id2 $ Dodo.words [ Dodo.text id1, Dodo.text "as", Dodo.text id2 ]

noPureAnns :: PrintOptions -> PrintOptions
noPureAnns = _ { pureAnns = false }

printPure :: forall a. PrintOptions -> Dodo.Doc a -> Dodo.Doc a
printPure { pureAnns } doc
  | pureAnns =
      Dodo.text "/* #__PURE__ */"
        <> Dodo.space
        <> doc
  | otherwise =
      doc

class ToEsIdent a where
  toEsIdent :: a -> EsIdent
  toEsIdentWith :: String -> a -> EsIdent

instance ToEsIdent ModuleName where
  toEsIdent (ModuleName mn) = Embedded (Ident mn) ""
  toEsIdentWith a = toEsIdentWith a <<< toEsIdent

instance ToEsIdent Ident where
  toEsIdent id = Embedded id ""
  toEsIdentWith = flip Embedded

instance ToEsIdent EsIdent where
  toEsIdent = identity
  toEsIdentWith a = case _ of
    Embedded id "" -> Embedded id a
    Embedded id b -> Embedded id (b <> "$" <> a)
    Generated id -> Generated (id <> "$" <> a)

esArrowFunction :: Array EsIdent -> Array EsExpr -> EsExpr
esArrowFunction args = build <<< EsArrowFunction args

esCurriedFunction :: Array EsIdent -> Array EsExpr -> EsExpr
esCurriedFunction args stmts = case Array.unsnoc args of
  Nothing ->
    esArrowFunction [] stmts
  Just { init, last } ->
    foldr (\a -> build <<< EsArrowFunction [ a ] <<< pure <<< build <<< EsReturn <<< Just) (build (EsArrowFunction [ last ] stmts)) init

esBinding :: EsIdent -> EsExpr -> EsExpr
esBinding ident expr = build $ EsConst $ NonEmptyArray.singleton $ Tuple (EsBindingIdent ident) expr

esLazyBinding :: EsExpr -> EsExpr
esLazyBinding = build <<< EsRuntime <<< EsBinding <<< build <<< EsArrowFunction [] <<< pure <<< build <<< EsReturn <<< Just

esAssignIdent :: EsIdent -> EsExpr -> EsExpr
esAssignIdent ident = build <<< EsAssign (build (EsIdent (Qualified Nothing ident)))
