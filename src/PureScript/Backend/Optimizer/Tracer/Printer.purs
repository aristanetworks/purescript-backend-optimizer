module PureScript.Backend.Optimizer.Tracer.Printer where

import Prelude

import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard, power)
import Data.Tuple (Tuple(..), snd, uncurry)
import Dodo (Doc)
import Dodo as D
import PureScript.Backend.Optimizer.Convert (OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..), propKey)
import PureScript.Backend.Optimizer.Semantics (BackendExpr, BackendRewrite(..), DistOp(..), UnpackOp(..), foldBackendExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

printSteps :: ModuleName -> OptimizationSteps -> Doc Void
printSteps modName allSteps = do
  let
    singleDashSeparator = D.text $ power "-" 15
    doubleDashSeparator = D.text $ power "=" 15
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
      | otherwise = D.lines [ acc, singleDashSeparator, renderSingleStep ident lastIdx idx step ]

    foldIdent ident acc steps
      | acc.idx == 0 =
          { idx: 1
          , value:
              foldlWithIndex (foldStep ident (NonEmptyArray.length steps - 1)) mempty $ NonEmptyArray.toArray steps
          }
      | otherwise =
          { idx: acc.idx + 1
          , value:
              D.foldWithSeparator (D.break <> D.break <> doubleDashSeparator <> D.break <> D.break)
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

printCurriedApp :: Doc Void -> Array (Doc Void) -> Doc Void
printCurriedApp fn args = D.flexAlt singleLine multiLine
  where
  singleLine = args # flip Array.foldl fn \acc next ->
    wrapInParens $ D.words [ acc, next ]

  multiLine =
    D.lines
      [ D.text "(" <> fn
      , D.indent $ D.lines args
      , D.text ")"
      ]

printCurriedAbs :: Array (Doc Void) -> Doc Void -> Doc Void
printCurriedAbs args body = D.flexAlt singleLine multiLine
  where
  printArg arg = do
    D.words
      [ D.text "\\" <> arg
      , D.text "->"
      ]
  singleLine = D.words
    [ D.text "(" <> (D.words $ map printArg args)
    , body <> D.text ")"
    ]
  multiLine = D.lines
    [ D.text "(" <> (D.words $ map printArg args)
    , D.indent body
    , D.text ")"
    ]

printUncurriedApp :: Boolean -> Doc Void -> Array (Doc Void) -> Doc Void
printUncurriedApp isEffectful fn args = do
  let effectfulChar = D.text "#" # guard isEffectful
  D.flexAlt
    ( D.words
        [ D.text "(" <> effectfulChar <> fn
        , D.words args <> effectfulChar <> D.text ")"
        ]
    )
    ( D.lines
        [ D.text "(" <> effectfulChar <> fn
        , D.indent $ D.lines args
        , effectfulChar <> D.text ")"
        ]
    )

printUncurriedAbs :: Boolean -> Array (Doc Void) -> Doc Void -> Doc Void
printUncurriedAbs isEffectful args body = do
  let effectfulChar = D.text "#" # guard isEffectful
  D.flexAlt
    ( D.words
        [ D.text "(" <> effectfulChar <> D.words [ D.text "\\" <> D.words args, D.text "->" ]
        , body <> effectfulChar <> D.text ")"
        ]
    )
    ( D.lines
        [ D.text "(" <> effectfulChar <> D.words [ D.text "\\" <> D.words args, D.text "->" ]
        , D.indent body
        , effectfulChar <> D.text ")"
        ]
    )

wrapIn :: String -> Doc Void -> Doc Void
wrapIn sep = wrapIn' sep sep

wrapIn' :: String -> String -> Doc Void -> Doc Void
wrapIn' l r = append (D.text l) <<< flip append (D.text r)

wrapInParens :: Doc Void -> Doc Void
wrapInParens = wrapIn' "(" ")"

printLet :: String -> Doc Void -> Doc Void -> Doc Void
printLet letKeyword = printLet' (D.text letKeyword)

printLet' :: Doc Void -> Doc Void -> Doc Void -> Doc Void
printLet' letKeyword identifier binding = D.flexGroup $
  D.paragraph
    [ D.words
        [ letKeyword
        , identifier
        , D.text "="
        ]
    , D.indent $ binding <> D.text ";"
    ]

printArray :: Array (Doc Void) -> Doc Void
printArray arr = D.flexGroup $ D.flexAlt (singleLine arr) (multiLine arr)
  where
  singleLine = flip foldrWithIndex (D.text " ]") \idx expr acc -> do
    let prefix = if idx == 0 then "[ " else ", "
    fold [ D.text prefix, expr, acc ]
  multiLine = flip foldrWithIndex (D.text "]") \idx expr acc -> do
    let prefix = if idx == 0 then D.text "[" else D.text ","
    D.lines [ D.words [ prefix, expr ], acc ]

printProp :: String -> Prop (Doc Void) -> Doc Void
printProp labelValueSep (Prop lbl expr) =
  D.words [ D.text lbl <> D.text labelValueSep, expr ]

printRecord :: String -> Array (Prop (Doc Void)) -> Doc Void
printRecord labelValueSep arr = D.flexGroup $
  D.flexAlt (singleLine arr) (D.flexAlt (multiLine arr) (multiLineIdentValues arr))
  where
  singleLine = flip foldrWithIndex (D.text "}") \idx prop acc -> do
    let prefix = if idx == 0 then "{ " else ", "
    fold
      [ D.text prefix
      , printProp labelValueSep prop
      , acc
      ]

  multiLine = flip foldrWithIndex (D.text "}") \idx prop acc -> do
    let prefix = if idx == 0 then "{" else ","
    D.lines
      [ D.words [ D.text prefix, printProp labelValueSep prop ]
      , acc
      ]

  multiLineIdentValues = flip foldrWithIndex (D.text "}") \idx (Prop lbl expr) acc -> do
    let prefix = if idx == 0 then "{" else ","
    D.lines
      [ D.words [ D.text prefix, D.text lbl <> D.text labelValueSep ]
      , D.indent $ D.indent expr
      , acc
      ]

printBranch :: NonEmptyArray (Pair (Doc Void)) -> Doc Void -> Doc Void
printBranch conds fallback =
  if NonEmptyArray.length conds == 1 then
    D.flexGroup $ D.flexAlt printSingleLine printMultiLine
  else
    printMultiLine
  where
  printCondHead ifKeyword cond = D.words [ D.text ifKeyword, cond, D.text "then" ]

  printSingleLine = do
    let Pair cond expr = NonEmptyArray.head conds
    D.words [ printCondHead "if" cond, expr, D.text "else", fallback ]

  printMultiLine = do
    let
      printCond idx (Pair cond expr) =
        D.lines
          [ printCondHead (if idx == 0 then "if" else "else if") cond
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
    ( \rewrite expr ->
        D.lines
          [ printBackendRewriteCase rewrite
          , expr
          ]
    )

printBackendSyntax :: BackendSyntax (Doc Void) -> Doc Void
printBackendSyntax = case _ of
  Var qi ->
    printQualifiedIdent qi
  Local mbIdent lvl ->
    printLocal mbIdent lvl
  Lit lit ->
    printLiteral lit
  App fn args ->
    printCurriedApp fn $ NonEmptyArray.toArray args
  Abs args body -> do
    printCurriedAbs (map (uncurry printLocal) $ NonEmptyArray.toArray args) body
  UncurriedApp fn args -> do
    printUncurriedApp false fn args

  UncurriedAbs args body ->
    printUncurriedAbs false (map (uncurry printLocal) args) body
  UncurriedEffectApp fn args -> do
    printUncurriedApp true fn args

  UncurriedEffectAbs args body ->
    printUncurriedAbs true (map (uncurry printLocal) args) body
  Accessor expr accessor ->
    D.flexGroup $ D.flexAlt
      (wrapInParens expr <> printBackendAccessor accessor)
      ( D.lines
          [ D.text "("
          , D.indent expr
          , D.text ")" <> printBackendAccessor accessor
          ]
      )
  Update expr propArr -> do
    D.flexGroup $ D.flexAlt
      ( D.words
          [ wrapInParens expr
          , printRecord "=" propArr
          ]
      )
      ( D.lines
          [ D.text "(" <> expr
          , D.indent $ D.text ")" <> printRecord "=" propArr
          ]
      )

  CtorSaturated qi _ ctorName _ values ->
    printUncurriedApp false
      (printQualifiedIdent qi <> D.text "." <> printProperName ctorName)
      (map snd values)

  CtorDef _ _ ctorName [] -> do
    printLet "letCtor" (printIdent ctorName)
      $ printRecord ":" [ Prop "tag" $ wrapIn "\"" $ printIdent ctorName ]

  CtorDef _ _ ctorName args -> do
    printLet "letCtor" (printIdent ctorName)
      $ printUncurriedAbs false (map D.text args)
      $ printRecord ":"
      $ Array.cons (Prop "tag" $ wrapIn "\"" $ printIdent ctorName)
      $ args <#> \arg -> Prop arg $ D.text arg

  LetRec lvl bindings body ->
    D.lines
      [ D.lines
          $ bindings <#>
              ( \(Tuple ident binding) ->
                  printLet' (D.text "letRec-" <> printLevel lvl) (printLocal (Just ident) lvl) binding
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
    printUncurriedApp true (D.text "effectPure") [ a ]

  EffectDefer a ->
    printUncurriedApp true (D.text "effectDefer") [ a ]

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
    D.text "-- Inline " <> (printLocal mbIdent lvl)

  RewriteUncurry mbIdent lvl _ _ _ ->
    D.text "-- Uncurry " <> (printLocal mbIdent lvl)

  RewriteLetAssoc _ _ ->
    D.text "-- LetAssoc"

  RewriteEffectBindAssoc _ _ ->
    D.text "-- EffectBindAssoc"

  RewriteStop _ ->
    D.text "-- Stop"

  RewriteUnpackOp mbIdent lvl unpackOp _ ->
    D.text "-- UnpackOp " <> (printLocal mbIdent lvl) <> D.space <> printUnpackOpCase unpackOp

  RewriteDistBranchesLet mbIdent lvl _ _ _ ->
    D.text "-- DistBranchesLet " <> (printLocal mbIdent lvl)

  RewriteDistBranchesOp _ _ distOp ->
    D.text "-- DistBranchesOp " <> printDistOpCase distOp

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
    D.text "App"
  DistUncurriedApp _ ->
    D.text "UncurriedApp"

  DistAccessor accessor ->
    D.words [ D.text "Accessor", printBackendAccessor accessor ]

  DistPrimOp1 op1 ->
    D.words [ D.text "PrimOp1", printBackendOperator1 op1 ]

  DistPrimOp2L op2 _ ->
    D.words [ D.text "PrimOp2 Left", printBackendOperator2 op2 ]

  DistPrimOp2R _ op2 ->
    D.words [ D.text "PrimOp2 Right", printBackendOperator2 op2 ]

printBackendEffect :: BackendEffect (Doc Void) -> Doc Void
printBackendEffect = case _ of
  EffectRefNew a ->
    printUncurriedApp true (D.text "refNew") [ a ]
  EffectRefRead a ->
    printUncurriedApp true (D.text "refRead") [ a ]
  EffectRefWrite a b ->
    printUncurriedApp true (D.text "refWrite") [ a, b ]

printBackendOperator :: BackendOperator (Doc Void) -> Doc Void
printBackendOperator = case _ of
  Op1 op1 a ->
    wrapInParens $ D.words [ printBackendOperator1 op1, a ]
  Op2 op2 l r ->
    wrapInParens $ D.words [ l, printBackendOperator2 op2, r ]

wrapInBackticks :: Doc Void -> Doc Void
wrapInBackticks = wrapIn "`"

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
  OpArrayIndex -> wrapInBackticks $ D.text "arrayIndex"
  OpBooleanAnd -> wrapInBackticks $ D.text "booleanAnd"
  OpBooleanOr -> wrapInBackticks $ D.text "booleanOr"
  OpBooleanOrd ord -> printBackendOperatorOrd ord
  OpCharOrd ord -> printBackendOperatorOrd ord
  OpIntBitAnd -> wrapInBackticks $ D.text "intBitAnd"
  OpIntBitOr -> wrapInBackticks $ D.text "intBitOr"
  OpIntBitShiftLeft -> wrapInBackticks $ D.text "intBitShiftLeft"
  OpIntBitShiftRight -> wrapInBackticks $ D.text "intBitShiftRight"
  OpIntBitXor -> wrapInBackticks $ D.text "intBitXor"
  OpIntBitZeroFillShiftRight -> wrapInBackticks $ D.text "intBitZeroFillShiftRight"
  OpIntNum num -> printBackendOperatorNum num
  OpIntOrd ord -> printBackendOperatorOrd ord
  OpNumberNum num -> printBackendOperatorNum num
  OpNumberOrd ord -> printBackendOperatorOrd ord
  OpStringAppend -> D.text "++"
  OpStringOrd ord -> printBackendOperatorOrd ord

printBackendOperatorNum :: BackendOperatorNum -> Doc Void
printBackendOperatorNum = case _ of
  OpAdd -> D.text "+"
  OpDivide -> D.text "/"
  OpMultiply -> D.text "*"
  OpSubtract -> D.text "-"

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
    D.text $ show c
  LitBoolean b ->
    D.text $ show b
  LitArray arr -> do
    printArray arr
  LitRecord propArr ->
    printRecord ":" propArr
