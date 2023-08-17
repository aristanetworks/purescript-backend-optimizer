module PureScript.Backend.Optimizer.Tracer.Printer
  ( printModuleSteps
  , printSteps
  ) where

import Prelude

import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard, power)
import Data.Tuple (Tuple(..), curry, fst, snd, uncurry)
import Dodo (Doc, foldWithSeparator)
import Dodo as D
import Dodo.Common as DC
import PureScript.Backend.Optimizer.Convert (OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (BackendExpr, BackendRewrite(..), DistOp(..), UnpackOp(..), foldBackendExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

heading :: forall a. String -> Doc a -> Doc a
heading repeat hd =
  D.words
    [ D.text (power repeat 2)
    , hd
    , D.withPosition \{ column, pageWidth } ->
        D.text (power repeat (pageWidth - column))
    ]

printModuleSteps :: ModuleName -> OptimizationSteps -> Doc Void
printModuleSteps (ModuleName mod) steps =
  fold
    [ heading "=" $ D.text mod
    , D.break
    , D.withPosition \{ pageWidth } ->
        D.text (power "=" pageWidth)
    , D.break
    , D.break
    , foldWithSeparator (D.break <> D.break) (uncurry printSteps <$> steps)
    ]

printSteps :: Qualified Ident -> NonEmptyArray BackendExpr -> Doc Void
printSteps qual steps = do
  let
    ident = printQualifiedIdent qual
    renderStep idx step = do
      D.lines
        [ heading (if idx == 0 then "+" else "-") $ D.words
            [ ident
            , D.text "Step"
            , D.text $ show $ idx + 1
            , case idx == 0, idx == NonEmptyArray.length steps - 1 of
                true, true -> D.text "(Original/Final)"
                true, _ -> D.text "(Original)"
                _, true -> D.text "(Final)"
                _, _ -> mempty
            ]
        , D.indent $ snd $ printBackendExpr step
        ]
  D.foldWithSeparator (D.break <> D.break) $ Array.mapWithIndex renderStep $ NonEmptyArray.toArray steps

printQualifiedIdent :: Qualified Ident -> Doc Void
printQualifiedIdent = printQualified printIdent

printQualified :: forall a. (a -> Doc Void) -> Qualified a -> Doc Void
printQualified printA (Qualified mbMod a) =
  maybe (printA a) (\(ModuleName mod) -> D.text mod <> D.text "." <> printA a) mbMod

printIdent :: Ident -> Doc Void
printIdent (Ident i) = D.text i

printLevel :: Level -> Doc Void
printLevel (Level l) = D.text $ show l

printLocal :: Maybe Ident -> Level -> Doc Void
printLocal mbIdent lvl = case mbIdent of
  Just (Ident "$__unused") ->
    D.text "_"
  _ ->
    foldMap printIdent mbIdent
      <> D.text "@"
      <> printLevel lvl

data Prec = PrecBlock | PrecApply | PrecAccess | PrecAtom

derive instance Eq Prec
derive instance Ord Prec

type PrecDoc = Tuple Prec (Doc Void)

wrapPrec :: Prec -> PrecDoc -> Doc Void
wrapPrec prec1 (Tuple prec2 doc)
  | prec2 < prec1 = DC.pursParens doc
  | otherwise = doc

printCurriedApp :: PrecDoc -> NonEmptyArray PrecDoc -> PrecDoc
printCurriedApp fn args =
  Tuple PrecApply
    $ D.flexGroup
    $ foldWithSeparator D.spaceBreak
    $ NonEmptyArray.cons (wrapPrec PrecApply fn) (D.indent <<< wrapPrec PrecAccess <$> args)

printCurriedAbs :: NonEmptyArray (Doc Void) -> PrecDoc -> PrecDoc
printCurriedAbs args body =
  Tuple PrecBlock $ D.flexGroup $ fold
    [ D.text "\\"
    , D.flexGroup $ D.alignCurrentColumn $ D.foldWithSeparator D.spaceBreak args
    , D.space
    , D.text "->"
    , D.spaceBreak
    , D.indent $ snd body
    ]

printUncurriedApp :: Boolean -> PrecDoc -> Array PrecDoc -> PrecDoc
printUncurriedApp isEffectful fn args =
  Tuple PrecApply $ D.flexGroup $ fold
    [ wrapPrec PrecAccess fn
    , D.softBreak
    , D.indent $ fold
        [ DC.pursParens $ D.foldWithSeparator DC.leadingComma $ snd <$> args
        , guard isEffectful $ D.text "!"
        ]
    ]

printUncurriedAbs :: Boolean -> Array (Doc Void) -> PrecDoc -> PrecDoc
printUncurriedAbs isEffectful args body =
  Tuple PrecBlock $ D.flexGroup $ fold
    [ D.text "\\"
    , D.flexGroup $ D.alignCurrentColumn $ DC.pursParens $ D.foldWithSeparator DC.leadingComma args
    , guard isEffectful $ D.text "!"
    , D.space
    , D.text "->"
    , D.spaceBreak
    , D.indent $ snd body
    ]

printLet :: String -> Doc Void -> Doc Void -> Doc Void
printLet letKeyword = curry (printLet' letKeyword <<< pure)

printLet' :: String -> Array (Tuple (Doc Void) (Doc Void)) -> Doc Void
printLet' letKeyword bindings =
  D.flexGroup $ fold
    [ D.text letKeyword
    , D.spaceBreak
    , D.indent $ D.lines $ map
        ( \(Tuple ident binding) ->
            D.flexGroup $ fold
              [ ident
              , D.space
              , D.text "="
              , D.spaceBreak
              , D.indent binding
              ]
        )
        bindings
    , D.space
    , D.text "in"
    ]

printArray :: Array (Doc Void) -> Doc Void
printArray = DC.pursSquares <<< D.foldWithSeparator DC.leadingComma

printRecord :: Doc Void -> Array (Prop (Doc Void)) -> Doc Void
printRecord sep = DC.pursCurlies <<< D.foldWithSeparator DC.leadingComma <<< map printProp
  where
  printProp (Prop prop doc) =
    D.flexGroup $ fold
      [ D.text (show prop)
      , sep
      , D.spaceBreak
      , D.align 2 $ D.indent doc
      ]

printBranch :: NonEmptyArray (Pair (Doc Void)) -> Doc Void -> Doc Void
printBranch conds fallback =
  foldr
    ( \(Pair a b) other ->
        fold
          [ D.flexGroup $ fold
              [ D.text "if"
              , D.spaceBreak
              , D.indent a
              , D.space
              , D.text "then"
              ]
          , D.indent b
          , D.break
          , D.text "else"
          , D.space
          , other
          ]
    )
    (D.break <> D.indent fallback)
    conds

printBackendExpr :: BackendExpr -> PrecDoc
printBackendExpr =
  foldBackendExpr
    printBackendSyntax
    ( \rewrite expr ->
        Tuple (fst expr) $ D.lines
          [ printBackendRewriteCase rewrite
          , snd expr
          ]
    )

printBackendSyntax :: BackendSyntax PrecDoc -> PrecDoc
printBackendSyntax = case _ of
  Var qi ->
    Tuple PrecAtom $ printQualifiedIdent qi
  Local mbIdent lvl ->
    Tuple PrecAtom $ printLocal mbIdent lvl
  Lit lit ->
    printLiteral lit
  App fn args ->
    printCurriedApp fn args
  Abs args body ->
    printCurriedAbs (map (uncurry printLocal) args) body
  UncurriedApp fn args ->
    printUncurriedApp false fn args
  UncurriedAbs args body ->
    printUncurriedAbs false (map (uncurry printLocal) args) body
  UncurriedEffectApp fn args ->
    printUncurriedApp true fn args
  UncurriedEffectAbs args body ->
    printUncurriedAbs true (map (uncurry printLocal) args) body
  Accessor expr accessor ->
    Tuple PrecAccess $ fold
      [ wrapPrec PrecAccess expr
      , printBackendAccessor accessor
      ]
  Update expr propArr ->
    Tuple PrecApply $ D.flexGroup $ fold
      [ wrapPrec PrecAccess expr
      , D.spaceBreak
      , D.indent $ printRecord (D.text " =") $ map snd <$> propArr
      ]
  CtorSaturated qi _ _ _ values ->
    printUncurriedApp false (Tuple PrecAtom (printQualifiedIdent qi)) (snd <$> values)
  CtorDef _ (ProperName proper) ctorName args ->
    Tuple PrecBlock $ D.words
      [ D.text "constructor"
      , printIdent ctorName <>
          D.encloseWithSeparator
            (D.text "(")
            (D.text ")")
            (D.text ", ")
            (Array.replicate (Array.length args) (D.text "_"))
      , D.text "of"
      , D.text proper
      ]
  LetRec lvl bindings body ->
    Tuple PrecBlock $ D.lines
      [ printLet' "letrec" $ NonEmptyArray.toArray $ map
          ( \(Tuple ident binding) ->
              Tuple (printLocal (Just ident) lvl) (snd binding)
          )
          bindings
      , snd body
      ]
  Let mbIdent lvl binding body ->
    Tuple PrecBlock $ D.lines
      [ printLet "let" (printLocal mbIdent lvl) $ snd binding
      , snd body
      ]
  EffectBind mbIdent lvl binding body ->
    Tuple PrecBlock $ D.lines
      [ printLet "let!" (printLocal mbIdent lvl) $ snd binding
      , snd body
      ]
  EffectPure a ->
    printUncurriedApp true (primOp "effect" "pure") [ a ]
  EffectDefer a ->
    printUncurriedApp true (primOp "effect" "defer") [ a ]
  Branch conds fallback ->
    Tuple PrecBlock $ printBranch (map (map snd) conds) $ snd fallback
  PrimOp op ->
    printBackendOperator op
  PrimEffect backendEffect ->
    printBackendEffect backendEffect
  PrimUndefined ->
    primOp "" "undefined"
  Fail _ ->
    printUncurriedApp false (primOp "" "fail") []

printBackendRewriteCase :: forall a. BackendRewrite a -> Doc Void
printBackendRewriteCase = case _ of
  RewriteInline _ _ _ _ ->
    printRewrite "Inline"
  RewriteUncurry _ _ _ _ _ ->
    printRewrite "Uncurry"
  RewriteStop _ ->
    printRewrite "Stop"
  RewriteUnpackOp _ _ unpackOp _ ->
    printUnpackOpCase unpackOp
  RewriteDistBranchesLet _ _ _ _ _ ->
    printRewrite "DistLet"
  RewriteDistBranchesOp _ _ distOp ->
    printDistOpCase distOp

printUnpackOpCase :: forall a. UnpackOp a -> Doc Void
printUnpackOpCase = case _ of
  UnpackRecord _ ->
    printRewrite "UnpackRecord"
  UnpackUpdate _ _ ->
    printRewrite "UnpackUpdate"
  UnpackArray _ ->
    printRewrite "UnpackArray"
  UnpackData _ _ _ _ _ ->
    printRewrite "UnpackData"

printDistOpCase :: forall a. DistOp a -> Doc Void
printDistOpCase = case _ of
  DistApp _ ->
    printRewrite "DistApp"
  DistUncurriedApp _ ->
    printRewrite "DistUncurriedApp"
  DistAccessor _ ->
    printRewrite "DistAccessor"
  DistPrimOp1 _ ->
    printRewrite "DistPrimOp1"
  DistPrimOp2L _ _ ->
    printRewrite "DistPrimOp2L"
  DistPrimOp2R _ _ ->
    printRewrite "DistPrimOp2R"

printRewrite :: String -> Doc Void
printRewrite str = D.text $ "{- " <> str <> " -}"

printBackendEffect :: BackendEffect PrecDoc -> PrecDoc
printBackendEffect = case _ of
  EffectRefNew a ->
    printUncurriedApp true (primOp "ref" "new") [ a ]
  EffectRefRead a ->
    printUncurriedApp true (primOp "ref" "read") [ a ]
  EffectRefWrite a b ->
    printUncurriedApp true (primOp "ref" "write") [ a, b ]

printBackendOperator :: BackendOperator PrecDoc -> PrecDoc
printBackendOperator = case _ of
  Op1 op1 a ->
    printUncurriedApp false (printBackendOperator1 op1) [ a ]
  Op2 op2 l r ->
    printUncurriedApp false (printBackendOperator2 op2) [ l, r ]

printBackendOperator1 :: BackendOperator1 -> PrecDoc
printBackendOperator1 = case _ of
  OpBooleanNot -> primOp "boolean" "not"
  OpIntBitNot -> primOp "int" "bitnot"
  OpIntNegate -> primOp "int" "negate"
  OpNumberNegate -> primOp "number" "negate"
  OpArrayLength -> primOp "array" "length"
  OpIsTag qi -> Tuple PrecAtom $ D.text "#[prim.istag " <> printQualifiedIdent qi <> D.text "]"

printBackendOperator2 :: BackendOperator2 -> PrecDoc
printBackendOperator2 = case _ of
  OpArrayIndex -> primOp "array" "index"
  OpBooleanAnd -> primOp "boolean" "and"
  OpBooleanOr -> primOp "boolean" "or"
  OpBooleanOrd ord -> printBackendOperatorOrd "boolean" ord
  OpCharOrd ord -> printBackendOperatorOrd "char" ord
  OpIntBitAnd -> primOp "int" "bitand"
  OpIntBitOr -> primOp "int" "bitor"
  OpIntBitShiftLeft -> primOp "int" "bitshl"
  OpIntBitShiftRight -> primOp "int" "bitshr"
  OpIntBitXor -> primOp "int" "bitxor"
  OpIntBitZeroFillShiftRight -> primOp "int" "bitzshr"
  OpIntNum num -> printBackendOperatorNum "int" num
  OpIntOrd ord -> printBackendOperatorOrd "int" ord
  OpNumberNum num -> printBackendOperatorNum "number" num
  OpNumberOrd ord -> printBackendOperatorOrd "number" ord
  OpStringAppend -> primOp "string" "append"
  OpStringOrd ord -> printBackendOperatorOrd "string" ord

primOp :: String -> String -> PrecDoc
primOp ns op
  | ns == "" =
      Tuple PrecAtom $ D.text $ "#[prim." <> op <> "]"
  | otherwise =
      Tuple PrecAtom $ D.text $ "#[prim." <> ns <> "." <> op <> "]"

printBackendOperatorNum :: String -> BackendOperatorNum -> PrecDoc
printBackendOperatorNum ns = primOp ns <<< case _ of
  OpAdd -> "add"
  OpDivide -> "div"
  OpMultiply -> "mul"
  OpSubtract -> "sub"

printBackendOperatorOrd :: String -> BackendOperatorOrd -> PrecDoc
printBackendOperatorOrd ns = primOp ns <<< case _ of
  OpEq -> "eq"
  OpNotEq -> "neq"
  OpGt -> "gt"
  OpGte -> "gte"
  OpLt -> "lt"
  OpLte -> "lte"

printBackendAccessor :: BackendAccessor -> Doc Void
printBackendAccessor = case _ of
  GetProp lbl ->
    D.text $ "." <> show lbl
  GetIndex i ->
    D.text $ "[" <> show i <> "]"
  GetCtorField _ _ _ _ _ ix ->
    D.text $ "#" <> show ix

printLiteral :: Literal PrecDoc -> PrecDoc
printLiteral = Tuple PrecAtom <<< case _ of
  LitInt i ->
    D.text $ show i
  LitNumber n ->
    D.text $ show n
  LitString s ->
    D.text $ show s
  LitChar c ->
    D.text $ show c
  LitBoolean b ->
    D.text $ show b
  LitArray arr -> do
    printArray $ map snd arr
  LitRecord propArr ->
    printRecord (D.text ":") $ map (map snd) propArr
