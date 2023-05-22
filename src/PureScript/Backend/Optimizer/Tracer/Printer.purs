module PureScript.Backend.Optimizer.Tracer.Printer where

import Prelude

import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, foldMap1)
import Data.Array.NonEmpty as NonEmptyArray
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard, power)
import Data.Tuple (Tuple(..), snd, uncurry)
import Dodo (Doc)
import Dodo as D
import PureScript.Backend.Optimizer.Convert (OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..), propKey)
import PureScript.Backend.Optimizer.Semantics (BackendExpr(..), BackendRewrite(..), DistOp(..), UnpackOp(..), foldBackendExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

printSteps :: ModuleName -> OptimizationSteps -> Doc Void
printSteps modName allSteps = do
  let
    renderSingleStep ident lastIdx idx step = do
      D.lines
        [ D.words
            [ printQualifiedIdent $ Qualified (Just modName) ident
            , wrapInParens $ fold
                [ D.words
                    [ D.text $ "Step"
                    , D.text $ show idx
                    ]
                , case idx == 0, idx == lastIdx of
                    true, true -> D.text "; Original/Final"
                    true, _ -> D.text "; Original"
                    _, true -> D.text "; Final"
                    _, _ -> mempty
                ]
            ]
        , D.indent $ printBackendExpr step
        ]
    foldStep ident lastIdx idx acc step
      | idx == 0 = renderSingleStep ident lastIdx idx step
      | otherwise = D.lines [ acc, D.text $ power "-" 15, renderSingleStep ident lastIdx idx step ]

    foldIdent ident acc steps
      | acc.idx == 0 =
          { idx: 1
          , value:
              foldlWithIndex (foldStep ident (NonEmptyArray.length steps - 1)) mempty $ NonEmptyArray.toArray steps
          }
      | otherwise =
          { idx: acc.idx + 1
          , value:
              D.foldWithSeparator (D.break <> D.break <> (D.text $ power "=" 15) <> D.break <> D.break)
                [ acc.value
                , foldlWithIndex (foldStep ident (NonEmptyArray.length steps - 1)) mempty $ NonEmptyArray.toArray steps
                ]
          }

  _.value $ foldlWithIndex foldIdent { idx: 0, value: mempty } allSteps

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
printLocal mbIdent lvl = do
  let lvl' = D.text "v" <> printLevel lvl
  maybe lvl' (\i -> printIdent i <> D.text "@" <> lvl') mbIdent

printProperName :: ProperName -> Doc Void
printProperName (ProperName s) = D.text s

printCurriedApp :: Doc Void -> Doc Void -> Doc Void
printCurriedApp fn arg =
  D.text "(" <> D.words [ fn, arg ] <> D.text ")"

printIndentedParens :: Doc Void -> Doc Void
printIndentedParens = printIndentedParens' mempty

printIndentedParens' :: Doc Void -> Doc Void -> Doc Void
printIndentedParens' header body =
  D.lines
    [ D.text "(" <> header
    , D.indent body
    , D.text ")"
    ]

wrapInParens :: Doc Void -> Doc Void
wrapInParens = append (D.text "(") <<< flip append (D.text ")")

printLet :: String -> Doc Void -> Doc Void -> Doc Void
printLet letKeyword identifier binding =
  D.lines
    [ D.words
        [ D.text letKeyword
        , identifier
        , D.text "="
        ]
    , D.indent $ binding <> D.text ";"
    ]

printArray :: Array (Doc Void) -> Doc Void
printArray = finish <<< start
  where
  finish topPart = D.lines [ topPart, D.text "]" ]
  start = flip foldlWithIndex mempty \idx acc expr ->
    if idx == 0 then
      D.lines
        [ D.text "["
        , D.indent $ expr <> D.text ","
        ]
    else
      D.lines
        [ acc
        , D.indent $ expr <> D.text ","
        ]

printProp :: forall a. (a -> Doc Void) -> Prop a -> Doc Void
printProp printExpr (Prop lbl expr) = D.words [ D.text lbl, D.text "=", printExpr expr ]

printRecord :: Array (Prop (Doc Void)) -> Doc Void
printRecord = finish <<< start
  where
  finish topPart = D.lines [ topPart, D.text "}" ]
  start = flip foldlWithIndex mempty \idx acc (Prop label expr) ->
    if idx == 0 then
      D.lines
        [ D.words [ D.text "{", D.text label, D.text "=" ]
        , D.indent $ D.indent expr
        ]
    else
      D.lines
        [ acc
        , D.words [ D.text ",", D.text label, D.text "=" ]
        , D.indent $ D.indent expr
        ]

printBranch :: NonEmptyArray (Pair (Doc Void)) -> Doc Void -> Doc Void
printBranch conds fallback = do
  let
    printCond idx (Pair cond expr) =
      D.lines
        [ D.words
            [ D.text $ if idx == 0 then "if" else "else if"
            , D.indent cond
            , D.text "then"
            ]
        , D.indent expr
        ]
  D.lines
    [ D.lines $ Array.mapWithIndex printCond (NonEmptyArray.toArray conds)
    , D.text "else"
    , D.indent fallback
    ]

printBackendExpr :: BackendExpr -> Doc Void
printBackendExpr =
  foldBackendExpr
    printBackendSyntax
    ( \rewrite rest ->
        D.lines
          [ printBackendRewriteCase rewrite
          , rest
          ]
    )
  where
  printBackendSyntax :: BackendSyntax (Doc Void) -> Doc Void
  printBackendSyntax = case _ of
    Var qi ->
      printQualifiedIdent qi
    Local mbIdent lvl ->
      printLocal mbIdent lvl
    Lit lit ->
      printLiteral lit
    App fn args ->
      (NonEmptyArray.toArray args) # flip Array.foldl fn \acc next ->
        printCurriedApp acc next
    Abs args body -> do
      let
        printArg arg =
          D.words
            [ D.text "\\" <> uncurry printLocal arg
            , D.text "->" <> D.space
            ]
      printIndentedParens' (foldMap1 printArg args) body
    UncurriedApp fn args ->
      wrapInParens $ D.words $ Array.cons fn args
    UncurriedAbs args body ->
      printIndentedParens'
        (D.text "\\" <> (D.words $ map (uncurry printLocal) args) <> D.space <> D.text "->")
        body
    UncurriedEffectApp fn args ->
      D.text "(#" <> (D.words $ Array.cons fn args) <> D.text "#)"
    UncurriedEffectAbs args body ->
      D.lines
        [ D.text "(#\\" <> (D.words $ map (uncurry printLocal) args) <> D.space <> D.text "->"
        , D.indent body
        , D.text "#)"
        ]
    Accessor expr accessor ->
      printIndentedParens expr <> printBackendAccessor accessor
    Update expr propArr -> do
      let sep = D.break <> D.text ", "
      printIndentedParens $
        D.lines
          [ expr
          , D.indent $ D.lines
              [ D.text "{"
              , D.foldWithSeparator sep $ map (printProp identity) propArr
              , D.text "}"
              ]
          ]

    CtorSaturated qi _ ctorName _ values ->
      wrapInParens $
        D.words
          [ printQualifiedIdent qi <> D.text "." <> printProperName ctorName
          , D.words $ map snd values
          ]

    CtorDef _ _ ctorName [] ->
      printLet "let" (printIdent ctorName)
        $ printRecord
            [ (Prop "tag" $ printLiteral $ LitString $ (\(Ident i) -> i) ctorName) ]

    CtorDef _ _ ctorName args ->
      printLet "let" (printIdent ctorName)
        $ printIndentedParens'
            (guard (not $ Array.null args) $ D.words [ D.text "\\", D.words $ map D.text args, D.text "->" ])
        $ printRecord
        $ Array.cons (Prop "tag" $ printLiteral $ LitString $ (\(Ident i) -> i) ctorName)
        $ args <#> \arg -> Prop arg $ printBackendExpr $ ExprSyntax mempty $ Var $ Qualified Nothing $ Ident arg

    LetRec lvl bindings body ->
      D.lines
        [ D.text "letrec"
        , D.indent
            $ D.lines
            $ bindings <#>
                ( \(Tuple ident binding) ->
                    printLet "let" (printLocal (Just ident) lvl) binding
                )
        , body
        ]

    Let mbIdent lvl binding body ->
      D.lines
        [ printLet "let" (printLocal mbIdent lvl) binding
        , body
        ]

    EffectBind mbIdent lvl binding body ->
      D.lines
        [ printLet "letEffect" (printLocal mbIdent lvl) binding
        , body
        ]
    EffectPure a ->
      wrapInParens $ D.words [ D.text "effectPure", a ]

    EffectDefer a ->
      wrapInParens $ D.words [ D.text "effectDefer", a ]

    Branch conds fallback ->
      printBranch conds fallback

    PrimOp op -> printBackendOperator op

    PrimEffect backendEffect -> printBackendEffect backendEffect

    PrimUndefined ->
      D.text "<PrimUndefined>"

    Fail _ ->
      D.text "<PatternMatchFailure>"

  printBackendRewriteCase :: BackendRewrite -> Doc Void
  printBackendRewriteCase = case _ of
    RewriteInline mbIdent lvl _ _ ->
      D.text "{#- Inline " <> (printLocal mbIdent lvl) <> D.text " -#}"

    RewriteUncurry mbIdent lvl _ _ _ ->
      D.text "{#- Uncurry " <> (printLocal mbIdent lvl) <> D.text " -#}"

    RewriteLetAssoc _ _ ->
      D.text "{#- LetAssoc -#}"

    RewriteEffectBindAssoc _ _ ->
      D.text "{#- EffectBindAssoc -#}"

    RewriteStop _ ->
      D.text "{#- Stop -#}"

    RewriteUnpackOp mbIdent lvl unpackOp _ ->
      D.text "{#- UnpackOp " <> (printLocal mbIdent lvl) <> D.space <> printUnpackOpCase unpackOp <> D.text " -#}"

    RewriteDistBranchesLet mbIdent lvl _ _ _ ->
      D.text "{#- DistBranchesLet " <> (printLocal mbIdent lvl) <> D.text " -#}"

    RewriteDistBranchesOp _ _ distOp ->
      D.text "{#- DistBranchesOp " <> printDistOpCase distOp <> D.text " -#}"

printUnpackOpCase :: UnpackOp -> Doc Void
printUnpackOpCase = case _ of
  UnpackRecord propArr ->
    D.text "Record " <> D.text (show $ map propKey propArr)
  UnpackUpdate _ propArr ->
    D.text "Update " <> D.text (show $ map propKey propArr)
  UnpackData _ _ tyName _ _ ->
    D.text "Data " <> printProperName tyName

printDistOpCase :: DistOp -> Doc Void
printDistOpCase = case _ of
  DistApp _ ->
    D.text "DistApp"
  DistUncurriedApp _ ->
    D.text "DistUncurriedApp"

  DistAccessor accessor ->
    D.words [ D.text "DistAccessor", printBackendAccessor accessor ]

  DistPrimOp1 op1 ->
    D.words [ D.text "DistPrimOp1", printBackendOperator1 op1 ]

  DistPrimOp2L op2 _ ->
    D.words [ D.text "DistPrimOp2L", printBackendOperator2 op2 ]

  DistPrimOp2R _ op2 ->
    D.words [ D.text "DistPrimOp2R", printBackendOperator2 op2 ]

printBackendEffect :: BackendEffect (Doc Void) -> Doc Void
printBackendEffect = case _ of
  EffectRefNew a ->
    wrapInParens $ D.words [ D.text "refNew", a ]
  EffectRefRead a ->
    wrapInParens $ D.words [ D.text "refRead", a ]
  EffectRefWrite a b ->
    wrapInParens $ D.words [ D.text "refWrite", a, b ]

printBackendOperator :: BackendOperator (Doc Void) -> Doc Void
printBackendOperator = case _ of
  Op1 op1 a ->
    wrapInParens $ D.words [ printBackendOperator1 op1, a ]
  Op2 op2 l r ->
    wrapInParens $ D.words [ printBackendOperator2 op2, l, r ]

printBackendOperator1 :: BackendOperator1 -> Doc Void
printBackendOperator1 = case _ of
  OpBooleanNot -> D.text "not"
  OpIntBitNot -> D.text "bitNot"
  OpIntNegate -> D.text "intNegate"
  OpNumberNegate -> D.text "numNegate"
  OpArrayLength -> D.text "arrayLength"
  OpIsTag qi -> D.words [ D.text "isTag", printQualifiedIdent qi ]

printBackendOperator2 :: BackendOperator2 -> Doc Void
printBackendOperator2 = case _ of
  OpArrayIndex -> D.text "arrayIndex"
  OpBooleanAnd -> D.text "booleanAnd"
  OpBooleanOr -> D.text "booleanOr"
  OpBooleanOrd ord -> printBackendOperatorOrd ord
  OpCharOrd ord -> printBackendOperatorOrd ord
  OpIntBitAnd -> D.text "intBitAnd"
  OpIntBitOr -> D.text "intBitOr"
  OpIntBitShiftLeft -> D.text "intBitShiftLeft"
  OpIntBitShiftRight -> D.text "intBitShiftRight"
  OpIntBitXor -> D.text "intBitXor"
  OpIntBitZeroFillShiftRight -> D.text "intBitZeroFillShiftRight"
  OpIntNum num -> printBackendOperatorNum num
  OpIntOrd ord -> printBackendOperatorOrd ord
  OpNumberNum num -> printBackendOperatorNum num
  OpNumberOrd ord -> printBackendOperatorOrd ord
  OpStringAppend -> D.text "stringAppend"
  OpStringOrd ord -> printBackendOperatorOrd ord

printBackendOperatorNum :: BackendOperatorNum -> Doc Void
printBackendOperatorNum = case _ of
  OpAdd -> D.text "add"
  OpDivide -> D.text "divide"
  OpMultiply -> D.text "multiply"
  OpSubtract -> D.text "subtract"

printBackendOperatorOrd :: BackendOperatorOrd -> Doc Void
printBackendOperatorOrd = case _ of
  OpEq -> D.text "=="
  OpNotEq -> D.text "/="
  OpGt -> D.text ">"
  OpGte -> D.text ">="
  OpLt -> D.text "<"
  OpLte -> D.text "<="

printBackendAccessor :: BackendAccessor -> Doc Void
printBackendAccessor = case _ of
  GetProp lbl ->
    D.text $ "." <> lbl
  GetIndex i ->
    D.text $ "[" <> show i <> "]"
  GetCtorField _ _ _ _ valueX _ ->
    D.text $ "#" <> valueX

printLiteral :: Literal (Doc Void) -> Doc Void
printLiteral = case _ of
  LitInt i ->
    D.text $ show i
  LitNumber n ->
    D.text $ show n
  LitString s ->
    D.text "\"" <> (D.text s) <> D.text "\""
  LitChar c ->
    D.text "'" <> (D.text $ show c) <> D.text "'"
  LitBoolean b ->
    D.text $ show b
  LitArray arr -> do
    printArray arr
  LitRecord propArr ->
    printRecord propArr
