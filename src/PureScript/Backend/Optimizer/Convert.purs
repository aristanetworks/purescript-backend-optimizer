-- | ### Algorithm Summary for Optimized Pattern Matching Conversion
-- |
-- | The algorithm used for converting `ExprCase` into `BackendExpr` is based on two papers:
-- | 1. https://www.cs.tufts.edu/comp/150FP/archive/luc-maranget/jun08.pdf - "Compiling Pattern Matching to Good Decision Trees" (CPMtGDT paper)
-- | 2. https://julesjacobs.com/notes/patternmatching/patternmatching.pdf - "How to compile pattern matching"
-- |
-- | The algorithm uses the composition of heuristics `p`, `b`, `a`, and pseudo-heuristic `N` described in
-- | the CPMtGDT paper.
-- |
-- | The algorithm can be summarized as:
-- |
-- | 1. Entry point preprocessing steps:
-- |     1. let-bind the expressions in the case head and refer to these as `caseHeadIdents`
-- |     2. for each case row, convert each column's `Binder` into its corresponding `Pattern` type, removing Newtypes completely
-- |     3. for each case row, zip the corresponding ident in `caseHeadIdents` with its corresponding `Pattern` value
-- |     4. calculate the references introduced in a case row's binders, sort them by their name, store the result with the case row expression, and reference these as `leafFnArgs`
-- |     5. let-bind the "leafs" in the case row's expression:
-- |         1. if `Unconditional`, let-bind the expression as a function, using `leafFnArgs` to determine the number, order, and names of the function args, and using the original expression as the function's body.
-- |         2. if `Guarded`, do the same let-bind-expression-as-function described in the `Unconditional` step for each `Guard` but do not yet convert the guards predicate. Since the references introduced by the case row's binders are not yet in scope, the predicate won't reference the correct values
-- | 2. Start the recursive algorithm
-- |     1. preprocess all record patterns as described previously, so that each case row's corresponding column has all fields referenced in that column and orders its fields in the same order throughout all case rows
-- |     2. If the clause matrix has 0 rows, then we produce a pattern match failure
-- |     3. Otherwise, there's at least 1 row. If the clause matrix's first row contains only wildcard patterns (e.g. `value is _`) or is otherwise empty
-- |         1. calculate the `allReferences` value by combining the case row's "References" array with the references introduced by each remaining column (if any)
-- |         2. Sort the `allReferences` array by the reference names, so that the order of the references matches the order originally calculated in `leafFnArgs`
-- |         3. case on the guard
-- |             1. if `Unconditional`, call the function it references with the ordered `allReferences` args
-- |             2. if `Guarded`
-- |                 1. add `allReferences` to the current scope and then convert the predicate.
-- |                 2. call the function it references with the ordered `allReferences` args
-- |     4. Otherwise, there's at least one column in the first row against which we still need to test (i.e. there is a `value is pattern` test where the `pattern` is not a wildcard/`_`).
-- |         1. From among the remaining non-wildcard patterns we could test, use a heuristic to determine which column's `value is pattern` test from the first row will produce the smallest tree
-- |             1. if the chosen column is a value that can always be expanded (e.g. a `Product` type or `Record` type), use that column
-- |             2. Otherwise, use heuristic `pbaN`
-- |         2. Build 2 new clause matrices, Problem A and Problem B, using the below rules. Problem A contains rows where a match occurred. Problem B contains rows where a match did not occur.
-- |             1. If a row's corresponding column uses the same pattern as the chosen one (e.g. `chosen: a is 1; row's: a is 1`), then
-- |                 1. in the case row, add the references row's corresponding pattern introduces the case rows' "References" array
-- |                 2. in the case row's columns, replace the parent pattern with its subterm patterns (if any)
-- |                 3. put the case row into Problem A because a match occurred
-- |             2. If a row's corresponding column differs from the chosen one (e.g. `chosen: a is 1; row's: a is 2`), then put it in Problem B; a match did not occur.
-- |             3. If a row's corresponding column is a wildcard (e.g. `chosen: a is 1; row's: a is _`)
-- |                 1. follow the instructions above as if there was a normal match and put the resulting row in Problem A
-- |                 2. put a copy of the row in Problem B
-- |         3. If the chosen column is an expandable type, recurse on Problem A
-- |         4. Otherwise, guard against the chosen pattern, recursing on Problem A if it succeeds and recursing on Problem B if it fails.
module PureScript.Backend.Optimizer.Convert where

import Prelude

import Control.Alternative (guard, (<|>))
import Control.Monad.RWS (ask)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap, foldl)
import Data.FoldableWithIndex (foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Monoid as Monoid
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Foldable (maximum)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (class Foldable, Accum, foldr, for, mapAccumL, mapAccumR, sequence, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.Backend.Optimizer.Analysis (BackendAnalysis)
import PureScript.Backend.Optimizer.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), Comment, ConstructorType(..), Expr(..), Guard(..), Ident(..), Literal(..), Meta(..), Module(..), ModuleName(..), ProperName, Qualified(..), ReExport, findProp, propKey, propValue, qualifiedModuleName, unQualified)
import PureScript.Backend.Optimizer.Directives (DirectiveHeaderResult, parseDirectiveHeader)
import PureScript.Backend.Optimizer.Semantics (BackendExpr(..), BackendSemantics, Ctx, DataTypeMeta, Env(..), EvalRef(..), ExternImpl(..), ExternSpine, InlineAccessor(..), InlineDirective(..), InlineDirectiveMap, NeutralExpr(..), build, evalExternFromImpl, evalExternRefFromImpl, freeze, optimize)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))
import PureScript.Backend.Optimizer.Utils (foldl1Array)
import Safe.Coerce (coerce)

type BackendBindingGroup a b =
  { recursive :: Boolean
  , bindings :: Array (Tuple a b)
  }

type BackendImplementations = Map (Qualified Ident) (Tuple BackendAnalysis ExternImpl)

type BackendModule =
  { name :: ModuleName
  , comments :: Array Comment
  , imports :: Set ModuleName
  , dataTypes :: Map ProperName DataTypeMeta
  , bindings :: Array (BackendBindingGroup Ident NeutralExpr)
  , exports :: Set Ident
  , reExports :: Set ReExport
  , foreign :: Set Ident
  , implementations :: BackendImplementations
  , directives :: InlineDirectiveMap
  }

type ConvertEnv =
  { currentLevel :: Int
  , currentModule :: ModuleName
  , dataTypes :: Map ProperName DataTypeMeta
  , toLevel :: Map Ident Level
  , implementations :: BackendImplementations
  , moduleImplementations :: BackendImplementations
  , optimizationSteps :: OptimizationSteps
  , directives :: InlineDirectiveMap
  , foreignSemantics :: Map (Qualified Ident) ForeignEval
  , rewriteLimit :: Int
  , traceIdents :: Set (Qualified Ident)
  }

type ConvertM = Function ConvertEnv

toBackendModule :: Module Ann -> ConvertM (Tuple OptimizationSteps BackendModule)
toBackendModule (Module mod) env = do
  let
    directives :: DirectiveHeaderResult
    directives = parseDirectiveHeader mod.name mod.comments

    ctors :: Array (Tuple ProperName (Tuple Ident (Array String)))
    ctors = do
      Binding _ _ value <- mod.decls >>= case _ of
        Rec bindings -> bindings
        NonRec binding -> pure binding
      case value of
        ExprConstructor _ dataTy ctor fields ->
          pure $ Tuple dataTy (Tuple ctor fields)
        _ -> []

    dataTypes :: Map ProperName DataTypeMeta
    dataTypes = ctors
      # Array.groupAllBy (comparing fst)
      # map
          ( \group -> do
              let proper = fst $ NonEmptyArray.head group
              let constructors = Map.fromFoldable $ mapWithIndex (\tag (Tuple _ (Tuple ctor fields)) -> Tuple ctor { fields, tag }) group
              let sizes = Array.length <<< snd <<< snd <$> group
              Tuple proper { constructors, size: maximum sizes }
          )
      # Map.fromFoldable

    moduleBindings :: Accum ConvertEnv (Array (BackendBindingGroup Ident (WithDeps NeutralExpr)))
    moduleBindings = toBackendTopLevelBindingGroups mod.decls env
      { dataTypes = dataTypes
      , directives =
          foldlWithIndex
            ( \qual dirs dir ->
                Map.alter (maybe (Just dir) Just) qual dirs
            )
            (Map.union directives.locals env.directives)
            directives.exports
      , moduleImplementations = Map.empty
      }

    localExports :: Set Ident
    localExports = Set.fromFoldable mod.exports

    isBindingUsed :: forall a. Set (Qualified Ident) -> Tuple Ident a -> Boolean
    isBindingUsed deps (Tuple ident _) = Set.member ident localExports || Set.member (Qualified (Just mod.name) ident) deps

    usedBindings :: Accum (Set (Qualified Ident)) (Array (BackendBindingGroup Ident NeutralExpr))
    usedBindings = mapAccumR
      ( \deps group -> do
          let
            { accum, value: newBindings } =
              if group.recursive then
                if Array.any (isBindingUsed deps) group.bindings then
                  { accum: foldMap (fst <<< snd) group.bindings <> deps
                  , value: (Just <<< map snd) <$> group.bindings
                  }
                else
                  { accum: deps, value: [] }
              else
                mapAccumR
                  ( \deps' binding@(Tuple ident (Tuple deps'' expr)) ->
                      if isBindingUsed deps' binding then
                        { accum: deps'' <> deps'
                        , value: Just (Tuple ident expr)
                        }
                      else
                        { accum: deps', value: Nothing }
                  )
                  deps
                  (group.bindings :: Array (Tuple Ident (Tuple _ NeutralExpr)))
          { accum
          , value: group { bindings = Array.catMaybes newBindings }
          }
      )
      Set.empty
      moduleBindings.value

    usedImports :: Set ModuleName
    usedImports = usedBindings.accum # Set.mapMaybe \qi -> do
      mn <- qualifiedModuleName qi
      mn <$ guard (mn /= mod.name && mn /= ModuleName "Prim")

  Tuple moduleBindings.accum.optimizationSteps $
    { name: mod.name
    , comments: mod.comments
    , imports: usedImports
    , dataTypes: Map.filter (Array.any (isBindingUsed usedBindings.accum) <<< Map.toUnfoldable <<< _.constructors) dataTypes
    , bindings: usedBindings.value
    , exports: localExports
    , reExports: Set.fromFoldable mod.reExports
    , implementations: moduleBindings.accum.moduleImplementations
    , directives: directives.exports
    , foreign: Set.fromFoldable mod.foreign
    }

type WithDeps = Tuple (Set (Qualified Ident))

toBackendTopLevelBindingGroups :: Array (Bind Ann) -> ConvertM (Accum ConvertEnv (Array (BackendBindingGroup Ident (WithDeps NeutralExpr))))
toBackendTopLevelBindingGroups binds env = do
  let result = mapAccumL toBackendTopLevelBindingGroup env binds
  result
    { value =
        (\as -> { recursive: (NonEmptyArray.head as).recursive, bindings: _.bindings =<< NonEmptyArray.toArray as }) <$>
          Array.groupBy ((&&) `on` (not <<< _.recursive)) result.value
    }

toBackendTopLevelBindingGroup :: ConvertEnv -> Bind Ann -> Accum ConvertEnv (BackendBindingGroup Ident (WithDeps NeutralExpr))
toBackendTopLevelBindingGroup env = case _ of
  Rec bindings -> do
    let group = (\(Binding _ ident _) -> Qualified (Just env.currentModule) ident) <$> bindings
    mapAccumL (toTopLevelBackendBinding group) env bindings
      # overValue { recursive: true, bindings: _ }
  NonRec binding ->
    mapAccumL (toTopLevelBackendBinding []) env [ binding ]
      # overValue { recursive: false, bindings: _ }
  where
  overValue f a =
    a { value = f a.value }

-- | For the NonEmptyArray,
-- | - `head` = the original expression
-- | - `last` = the final optimized expression
-- | - everything in-between the two are the steps that were taken from `head` to `last`
type OptimizationSteps = Array (Tuple (Qualified Ident) (NonEmptyArray BackendExpr))

toTopLevelBackendBinding :: Array (Qualified Ident) -> ConvertEnv -> Binding Ann -> Accum ConvertEnv (Tuple Ident (WithDeps NeutralExpr))
toTopLevelBackendBinding group env (Binding _ ident cfn) = do
  let evalEnv = Env { currentModule: env.currentModule, evalExternRef: makeExternEvalRef env, evalExternSpine: makeExternEvalSpine env, locals: [], directives: env.directives }
  let qualifiedIdent = Qualified (Just env.currentModule) ident
  let backendExpr = toBackendExpr cfn env
  let enableTracing = Set.member qualifiedIdent env.traceIdents
  let Tuple mbSteps optimizedExpr = optimize enableTracing (getCtx env) evalEnv qualifiedIdent env.rewriteLimit backendExpr
  let Tuple impl expr' = toExternImpl env group optimizedExpr
  { accum: env
      { implementations = Map.insert qualifiedIdent impl env.implementations
      , moduleImplementations = Map.insert qualifiedIdent impl env.moduleImplementations
      , optimizationSteps = maybe env.optimizationSteps (Array.snoc env.optimizationSteps <<< Tuple qualifiedIdent) $ NonEmptyArray.fromArray mbSteps
      , directives =
          case inferTransitiveDirective env.directives (snd impl) backendExpr cfn of
            Just dirs ->
              Map.alter
                case _ of
                  Just oldDirs ->
                    Just $ Map.union oldDirs dirs
                  Nothing ->
                    Just dirs
                (EvalExtern (Qualified (Just env.currentModule) ident))
                env.directives
            Nothing ->
              env.directives
      }
  , value: Tuple ident (Tuple (unwrap (fst impl)).deps expr')
  }

inferTransitiveDirective :: InlineDirectiveMap -> ExternImpl -> BackendExpr -> Expr Ann -> Maybe (Map InlineAccessor InlineDirective)
inferTransitiveDirective directives impl backendExpr cfn = fromImpl <|> fromBackendExpr
  where
  fromImpl = case impl of
    ExternExpr _ (NeutralExpr (App (NeutralExpr (Var qual)) args)) ->
      case Map.lookup (EvalExtern qual) directives of
        Just dirs -> do
          let
            newDirs = foldrWithIndex
              ( \ix dir accum -> case ix, dir of
                  InlineRef, (InlineArity n) ->
                    accum
                      # Map.insert InlineRef (InlineArity (n - NonEmptyArray.length args))
                  InlineSpineProp prop, _ ->
                    accum
                      # Map.insert (InlineProp prop) dir
                      # Map.insert (InlineSpineProp prop) dir
                  _, _ ->
                    accum
              )
              Map.empty
              dirs
          if Map.isEmpty newDirs then
            Nothing
          else
            Just newDirs
        _ ->
          Nothing
    ExternExpr _ (NeutralExpr (Accessor (NeutralExpr (App (NeutralExpr (Var qual)) _)) (GetProp prop))) ->
      case Map.lookup (EvalExtern qual) directives >>= Map.lookup (InlineSpineProp prop) of
        Just (InlineArity n) ->
          Just $ Map.singleton InlineRef (InlineArity n)
        _ ->
          Nothing
    _ ->
      Nothing

  fromBackendExpr = case backendExpr of
    ExprSyntax _ (App (ExprSyntax _ (Var qual)) args) ->
      case Map.lookup (EvalExtern qual) directives >>= Map.lookup InlineRef of
        Just (InlineArity n)
          | ExprApp (Ann { meta: Just IsSyntheticApp }) _ _ <- cfn
          , arity <- NonEmptyArray.length args
          , arity >= n ->
              Just $ Map.singleton InlineRef InlineAlways
        _ ->
          Nothing
    _ ->
      Nothing

toExternImpl :: ConvertEnv -> Array (Qualified Ident) -> BackendExpr -> Tuple (Tuple BackendAnalysis ExternImpl) NeutralExpr
toExternImpl env group expr = case expr of
  ExprSyntax analysis (Lit (LitRecord props)) -> do
    let propsWithAnalysis = map freeze <$> props
    Tuple (Tuple analysis (ExternDict group propsWithAnalysis)) (NeutralExpr (Lit (LitRecord (map snd <$> propsWithAnalysis))))
  ExprSyntax _ (CtorDef ct ty tag fields) -> do
    let Tuple analysis expr' = freeze expr
    let meta = unsafePartial $ fromJust $ Map.lookup ty env.dataTypes
    Tuple (Tuple analysis (ExternCtor meta ct ty tag fields)) expr'
  _ -> do
    let Tuple analysis expr' = freeze expr
    Tuple (Tuple analysis (ExternExpr group expr')) expr'

topEnv :: Env -> Env
topEnv (Env env) = Env env { locals = [] }

makeExternEvalSpine :: ConvertEnv -> Env -> Qualified Ident -> Array ExternSpine -> Maybe BackendSemantics
makeExternEvalSpine conv env qual spine = do
  let
    result = do
      fn <- Map.lookup qual conv.foreignSemantics
      fn env qual spine
  case result of
    Nothing -> do
      impl <- Map.lookup qual conv.implementations
      evalExternFromImpl (topEnv env) qual impl spine
    _ ->
      result

makeExternEvalRef :: ConvertEnv -> Env -> Qualified Ident -> Maybe BackendSemantics
makeExternEvalRef conv env qual =
  evalExternRefFromImpl env qual <$> Map.lookup qual conv.implementations

buildM :: BackendSyntax BackendExpr -> ConvertM BackendExpr
buildM a env = build (getCtx env) a

getCtx :: ConvertEnv -> Ctx
getCtx env =
  { currentLevel: env.currentLevel
  , lookupExtern
  , effect: false
  }
  where
  lookupExtern (Tuple qual acc) = do
    Tuple s impl <- Map.lookup qual env.implementations
    case impl of
      ExternExpr _ a ->
        case acc of
          Nothing ->
            Just (Tuple s a)
          _ ->
            Nothing
      ExternDict _ a ->
        case acc of
          Just (GetProp prop) ->
            findProp prop a
          -- Nothing ->
          --   Just $ Tuple s $ NeutralExpr $ Lit $ LitRecord (map snd <$> a)
          _ ->
            Nothing
      ExternCtor _ _ _ _ _ ->
        Nothing

fromExternImpl :: ExternImpl -> Maybe NeutralExpr
fromExternImpl = case _ of
  ExternExpr _ a -> Just a
  ExternDict _ _ -> Nothing
  ExternCtor _ _ _ _ _ -> Nothing

levelUp :: forall a. ConvertM a -> ConvertM a
levelUp f env = f (env { currentLevel = env.currentLevel + 1 })

intro :: forall f a. Foldable f => f Ident -> Level -> ConvertM a -> ConvertM a
intro ident lvl f env = f
  ( env
      { currentLevel = env.currentLevel + 1
      , toLevel = foldr (flip Map.insert lvl) env.toLevel ident
      }
  )

currentLevel :: ConvertM Level
currentLevel env = Level env.currentLevel

toBackendExpr :: Expr Ann -> ConvertM BackendExpr
toBackendExpr = case _ of
  ExprVar _ qi -> do
    { currentModule, toLevel } <- ask
    case qi of
      Qualified Nothing ident | Just lvl <- Map.lookup ident toLevel ->
        buildM (Local (Just ident) lvl)
      Qualified (Just mn) ident | mn == currentModule, Just lvl <- Map.lookup ident toLevel ->
        buildM (Local (Just ident) lvl)
      Qualified (Just (ModuleName "Prim")) (Ident "undefined") ->
        buildM PrimUndefined
      Qualified Nothing ident ->
        buildM (Var (Qualified (Just currentModule) ident))
      _ ->
        buildM (Var qi)
  ExprLit _ lit ->
    buildM <<< Lit =<< traverse toBackendExpr lit
  ExprConstructor _ ty name fields -> do
    { dataTypes } <- ask
    let
      ct = case Map.lookup ty dataTypes of
        Just { constructors } | Map.size constructors == 1 -> ProductType
        _ -> SumType
    buildM (CtorDef ct ty name fields)
  ExprAccessor _ a field ->
    buildM <<< flip Accessor (GetProp field) =<< toBackendExpr a
  ExprUpdate _ a bs ->
    join $ (\x y -> buildM (Update x y))
      <$> toBackendExpr a
      <*> traverse (traverse toBackendExpr) bs
  ExprAbs _ arg body -> do
    lvl <- currentLevel
    make $ Abs (NonEmptyArray.singleton (Tuple (Just arg) lvl)) (intro [ arg ] lvl (toBackendExpr body))
  ExprApp _ a b
    | ExprVar (Ann { meta: Just IsNewtype }) id <- a -> do
        toBackendExpr b
    | otherwise ->
        make $ App (toBackendExpr a) (NonEmptyArray.singleton (toBackendExpr b))
  ExprLet _ binds body ->
    foldr go (toBackendExpr body) binds
    where
    go bind' next = case bind' of
      NonRec (Binding _ ident expr) ->
        makeLet (Just ident) (toBackendExpr expr) \_ -> next
      Rec bindings | Just bindings' <- NonEmptyArray.fromArray bindings -> do
        lvl <- currentLevel
        let idents = (\(Binding _ ident _) -> ident) <$> bindings'
        join $ (\x y -> buildM (LetRec lvl x y))
          <$> intro idents lvl (traverse toBackendBinding bindings')
          <*> intro idents lvl next
      Rec _ ->
        unsafeCrashWith "CoreFn empty Rec binding group"
  ExprCase _ exprs alts ->
    foldr
      ( \expr next idents ->
          makeLet Nothing (toBackendExpr expr) \tmp ->
            next (Array.snoc idents tmp)
      )
      ( \idents ->
          toInitialCaseRows idents alts \caseRows ->
            buildCaseTreeFromRows caseRows
      )
      exprs
      []
  where
  toInitialCaseRows :: Array Level -> Array (CaseAlternative Ann) -> (Array CaseRow -> ConvertM BackendExpr) -> ConvertM BackendExpr
  toInitialCaseRows idents alts useCaseRowsCb =
    foldr
      ( \(CaseAlternative bs g) mainCb caseRows -> do
          patterns <- Array.zipWithA (\ident b -> { column: ident, pattern: _ } <$> binderToPattern b) idents bs
          let
            args = Array.sort $ foldMap patternVars patterns
            buildCaseRow guardFn = { patterns, guardFn, vars: SemigroupMap Map.empty }

          case g of
            Unconditional e ->
              makeLet Nothing (makeUncurriedAbs args (\_ -> toBackendExpr e)) \tmp ->
                mainCb $ Array.snoc caseRows $ buildCaseRow $ UnconditionalFn tmp
            Guarded gs ->
              foldr
                ( \(Guard pred body) cb xs ->
                    makeLet Nothing (makeUncurriedAbs args (\_ -> toBackendExpr body)) \tmp ->
                      cb $ Array.snoc xs (Tuple pred tmp)
                )
                ( \xs ->
                    case NonEmptyArray.fromArray xs of
                      Nothing -> unsafeCrashWith "CoreFn empty Guarded"
                      Just xs' ->
                        mainCb $ Array.snoc caseRows $ buildCaseRow $ GuardedFn xs'
                )
                gs
                []
      )
      useCaseRowsCb
      alts
      []

data CaseRowGuardedExpr
  = UnconditionalFn Level
  | GuardedFn (NonEmptyArray (Tuple (Expr Ann) Level))

-- guard - the code to run if the pattern matches
-- patterns - the remaining patterns to match
-- vars - the references introduced by binders thus far into the pattern matching
type CaseRow =
  { guardFn :: CaseRowGuardedExpr
  , vars :: SemigroupMap Ident (First Level)
  , patterns :: Array TopPattern
  }

-- | column - the original identifier against which this pattern matches
type TopPattern =
  { column :: Level
  , pattern :: Pattern
  }

-- | accessor - how to access this subterm from the parent expression
type SubPattern =
  { accessor :: BackendAccessor
  , pattern :: Pattern
  }

-- | vars - the references introduced at this pattern.
-- | pattern - the actual pattern match to test
-- | subterms - the subterm patterns to match only once this pattern matches.
newtype Pattern = Pattern
  { vars :: Set Ident
  , patternCase :: PatternCase
  , subterms :: Array SubPattern
  }

derive instance Newtype Pattern _

data PatternCase
  = PatWild
  | PatRecord (Array String)
  | PatProduct (Qualified ProperName) (Qualified Ident)
  | PatArray Int
  | PatSum (Qualified ProperName) (Qualified Ident)
  | PatInt Int
  | PatNumber Number
  | PatString String
  | PatChar Char
  | PatBoolean Boolean

derive instance Eq PatternCase
derive instance Ord PatternCase

binderToPattern :: Binder Ann -> ConvertM Pattern
binderToPattern = case _ of
  BinderNull _ -> primitivePattern PatWild
  BinderVar _ var ->
    pure $ Pattern { vars: Set.singleton var, patternCase: PatWild, subterms: [] }
  BinderNamed _ var next ->
    map (over Pattern \r -> r { vars = Set.insert var r.vars }) $ binderToPattern next
  BinderLit _ lit -> case lit of
    LitInt a -> primitivePattern $ PatInt a
    LitNumber a -> primitivePattern $ PatNumber a
    LitString a -> primitivePattern $ PatString a
    LitChar a -> primitivePattern $ PatChar a
    LitBoolean a -> primitivePattern $ PatBoolean a
    LitArray vals ->
      ctorPattern
        (PatArray $ Array.length vals)
        vals
        (\idx _ -> GetIndex idx)
        identity
    LitRecord fields ->
      -- We cannot safely expand the fields here because the number of columns
      -- would change. So, any later rows' `BinderNull` or `BinderVar` would not similarly be expanded.
      -- Moreover, we still need to add missing fields and then sort them.
      ctorPattern
        (PatRecord $ map propKey fields)
        fields
        (\_ p -> GetProp $ propKey p)
        propValue
  BinderConstructor (Ann { meta }) tyName ctorName args -> case meta of
    Just IsNewtype
      | [ arg ] <- args ->
          -- We can safely expand the subterm here because the number of columns
          -- remains the same here.
          binderToPattern arg
      | otherwise ->
          unsafeCrashWith "Newtype binder didn't wrap 1 arg"
    Just (IsConstructor ProductType _) -> do
      ctorFields <- lookupCtorFields tyName ctorName
      let argsWithNames = Array.zip args ctorFields
      -- We cannot safely expand the fields here because the number of columns
      -- would change. So, any later rows' `BinderNull` or `BinderVar` would not similarly be expanded.
      ctorPattern
        (PatProduct tyName ctorName)
        argsWithNames
        (\idx (Tuple _ fieldName) -> GetCtorField ctorName ProductType (unQualified tyName) (unQualified ctorName) fieldName idx)
        fst
    Just (IsConstructor SumType _) -> do
      ctorFields <- lookupCtorFields tyName ctorName
      let argsWithNames = Array.zip args ctorFields
      ctorPattern
        (PatSum tyName ctorName)
        argsWithNames
        (\idx (Tuple _ fieldName) -> GetCtorField ctorName SumType (unQualified tyName) (unQualified ctorName) fieldName idx)
        fst
    _ ->
      unsafeCrashWith "binderToPattern - invalid meta"
  where
  primitivePattern :: PatternCase -> ConvertM Pattern
  primitivePattern patternCase = pure $ Pattern { vars: Set.empty, patternCase, subterms: [] }

  ctorPattern
    :: forall a
     . PatternCase
    -> Array a
    -> (Int -> a -> BackendAccessor)
    -> (a -> Binder Ann)
    -> ConvertM Pattern
  ctorPattern patternCase args buildAccessor toBinder = ado
    subterms <- forWithIndex args \idx nextArg -> ado
      pattern <- binderToPattern $ toBinder nextArg
      in { accessor: buildAccessor idx nextArg, pattern }
    in
      Pattern
        { vars: Set.empty
        , patternCase
        , subterms
        }

  lookupCtorFields
    :: Qualified ProperName
    -> Qualified Ident
    -> ConvertM (Array String)
  lookupCtorFields ty ctor = do
    { dataTypes, implementations } <- ask
    case importedCtorFields implementations <|> localCtorFields dataTypes of
      Just fields -> pure fields
      Nothing -> unsafeCrashWith "Invariant broken: could not determine pattern matched constructor's fields during conversion."
    where
    importedCtorFields implementations = case Map.lookup ctor implementations of
      Just (Tuple _ (ExternCtor _ _ _ _ fields)) -> Just fields
      _ -> Nothing

    localCtorFields dataTypes = do
      { constructors } <- Map.lookup (unQualified ty) dataTypes
      _.fields <$> Map.lookup (unQualified ctor) constructors

patternVars :: forall r. { pattern :: Pattern | r } -> Array Ident
patternVars { pattern: Pattern { vars, subterms } } =
  Set.toUnfoldable vars <> foldMap patternVars subterms

toCaseRowVars :: TopPattern -> SemigroupMap Ident (First Level)
toCaseRowVars { column, pattern: Pattern p } = foldMap (SemigroupMap <<< flip Map.singleton (First column)) p.vars

-- `patternCase` has a naming clash with record puns
patternPatCase :: forall r. { pattern :: Pattern | r } -> PatternCase
patternPatCase { pattern: Pattern r } = r.patternCase

patternSubterms :: forall r. { pattern :: Pattern | r } -> Array SubPattern
patternSubterms { pattern: Pattern r } = r.subterms

buildCaseTreeFromRows :: Array CaseRow -> ConvertM BackendExpr
buildCaseTreeFromRows denormalizedRows = case NonEmptyArray.fromArray $ normalizeCaseRows denormalizedRows of
  Nothing ->
    patternFail
  Just rows -> do
    let
      { head: row0, tail } = NonEmptyArray.uncons rows
      row0NonPatWildPatterns =
        NonEmptyArray.fromArray
          $ foldlWithIndex (\idx acc p -> if patternPatCase p /= PatWild then Array.snoc acc (Tuple idx p) else acc) []
          $ row0.patterns

    case row0NonPatWildPatterns of
      Nothing ->
        buildCaseLeaf row0 tail
      Just neaRow0Patterns ->
        buildCasePattern (chooseNextPattern neaRow0Patterns tail) $ NonEmptyArray.toArray rows

normalizeCaseRows :: Array CaseRow -> Array CaseRow
normalizeCaseRows = normalizeProps =<< columnProps
  where
  columnProps :: Array CaseRow -> Array (Set String)
  columnProps caseRows = go 0 []
    where
    go :: Int -> Array (Set String) -> Array (Set String)
    go columnIdx columnsAcc =
      case nextColumnFields of
        Nothing -> columnsAcc
        Just a -> go (columnIdx + 1) (Array.snoc columnsAcc a)
      where
      nextColumnFields = caseRows # flip foldl Nothing \acc next -> do
        pat <- Array.index next.patterns columnIdx
        pure case patternPatCase pat of
          PatRecord fields -> do
            let keys = Set.fromFoldable fields
            maybe keys (append keys) acc
          _ ->
            fromMaybe Set.empty acc

  normalizeProps :: Array (Set String) -> Array CaseRow -> Array CaseRow
  normalizeProps allFieldNames = map \nextRow ->
    nextRow { patterns = Array.zipWith addBinders allFieldNames nextRow.patterns }
    where
    addBinders allFieldsSet pat = case patternPatCase pat of
      PatRecord fields -> do
        let
          currentFieldsWithSubterms = Array.zip fields $ patternSubterms pat
          allFieldsWithWildSubterms = (Set.toUnfoldable allFieldsSet) <#> \fieldName ->
            Tuple fieldName $ { accessor: GetProp fieldName, pattern: Pattern { vars: Set.empty, patternCase: PatWild, subterms: [] } }
          Tuple allFields allSubterms =
            Array.unzip
              $ map NonEmptyArray.head
              $ Array.groupAllBy (comparing fst)
              $ currentFieldsWithSubterms <> allFieldsWithWildSubterms
        pat { pattern = over Pattern (_ { patternCase = PatRecord allFields, subterms = allSubterms }) pat.pattern }
      _ -> pat

buildCaseLeaf :: CaseRow -> Array CaseRow -> ConvertM BackendExpr
buildCaseLeaf row0 tailRows = do
  let
    orderedArgs =
      Map.toUnfoldable
        $ (coerce :: forall k v. SemigroupMap k (First v) -> Map k v)
        $ row0.vars <> foldMap toCaseRowVars row0.patterns
    callFn fn args =
      make $ UncurriedApp (make $ Local Nothing fn) (map (\(Tuple i l) -> make $ Local (Just i) l) args)

  case row0.guardFn of
    UnconditionalFn fn ->
      callFn fn orderedArgs
    GuardedFn gs ->
      -- Note: `orderedArgs` are the references introduced by the binders for this row
      -- that have already been let-bound, but not using their source-code name.
      -- The output of the predicates below will be nonsensical unless these references
      -- are let-bound again with their source-code name.
      -- Duplicate let-bound variables will be inlined to at most one let-bound
      -- variable by the inliner (if they aren't eliminated altogether).
      foldr
        ( \(Tuple i l) cb args ->
            makeLet (Just i) (make $ Local Nothing l) \tmp ->
              cb $ Array.snoc args (Tuple i tmp)
        )
        ( \args -> do
            pairs <- for gs \(Tuple pred bodyFn) ->
              Pair <$> toBackendExpr pred <*> callFn bodyFn args
            fallback <- buildCaseTreeFromRows tailRows
            buildM $ Branch pairs fallback
        )
        orderedArgs
        []

chooseNextPattern :: forall r. NonEmptyArray (Tuple Int TopPattern) -> Array { patterns :: Array TopPattern | r } -> TopPattern
chooseNextPattern row0Patterns tailRows =
  case expandIfPossible of
    Just a -> a
    Nothing -> do
      let
        matchingPatternGroups :: NonEmptyArray { pattern :: TopPattern, pScore :: Int, bScore :: Int, aScore :: Int }
        matchingPatternGroups = row0Patterns <#> \(Tuple colIdx pat) -> do
          let
            matchingCols :: { tailRowIndices :: Array Int, ctors :: Set PatternCase, aScore :: Additive Int }
            matchingCols = tailRows # foldMapWithIndex \rowIdx row ->
              case Array.index row.patterns colIdx of
                Nothing ->
                  unsafeCrashWith "Impossible: rows' column lengths differ in pattern match"
                Just tailRowPat ->
                  { tailRowIndices: Monoid.guard (on eq patternPatCase pat tailRowPat) [ rowIdx + 1 ]
                  , ctors: Monoid.guard (patternPatCase tailRowPat /= PatWild) $ Set.singleton $ patternPatCase tailRowPat
                  , aScore: Additive $ negate $ Array.length $ Array.filter (notEq PatWild <<< patternPatCase) $ patternSubterms tailRowPat
                  }
          { pattern: pat
          , pScore: foldl (\l r -> if l + 1 == r then r else l) 0 matchingCols.tailRowIndices
          , bScore: negate $ Set.size $ Set.insert (patternPatCase pat) matchingCols.ctors
          , aScore: unwrap matchingCols.aScore
          }
        heuristic =
          maximumByAll (comparing _.pScore)
            >=> maximumByAll (comparing _.bScore)
            >=> maximumByAll (comparing _.aScore)
              >>> map (_.pattern <<< NonEmptyArray.head)
      case heuristic matchingPatternGroups of
        Just a -> a
        Nothing -> snd $ NonEmptyArray.head row0Patterns
  where
  maximumByAll :: forall f a. Foldable f => (a -> a -> Ordering) -> f a -> Maybe (NonEmptyArray a)
  maximumByAll f = foldl keepAllMax Nothing
    where
    keepAllMax acc next =
      case acc of
        Nothing -> Just $ NonEmptyArray.singleton next
        Just a -> case f (NonEmptyArray.head a) next of
          GT -> acc
          EQ -> Just $ NonEmptyArray.snoc a next
          LT -> Just $ NonEmptyArray.singleton next

  expandIfPossible :: Maybe TopPattern
  expandIfPossible = row0Patterns # NonEmptyArray.findMap \(Tuple _ next) -> case patternPatCase next of
    PatRecord _ -> Just next
    PatProduct _ _ -> Just next
    _ -> Nothing

buildCasePattern :: TopPattern -> Array CaseRow -> ConvertM BackendExpr
buildCasePattern chosenColumn rows = case patternPatCase chosenColumn of
  PatWild ->
    unsafeCrashWith "Impossible: chosen column cannot be wild pattern"
  PatRecord _ ->
    expandSubterms
  PatProduct _ _ ->
    expandSubterms
  PatSum _ a ->
    buildCaseBranch (guardTag a)
  PatArray a ->
    buildCaseBranch (guardArrayLength a)
  PatInt a ->
    buildCaseBranch (guardInt a)
  PatNumber a ->
    buildCaseBranch (guardNumber a)
  PatString a ->
    buildCaseBranch (guardString a)
  PatChar a ->
    buildCaseBranch (guardChar a)
  PatBoolean a ->
    buildCaseBranch (guardBoolean a)
  where
  -- There's no guard to make here. We just expose all subterms as patterns in the following expressions.
  expandSubterms :: ConvertM BackendExpr
  expandSubterms = do
    let { rowsWithMatch } = decompose chosenColumn rows
    foldr
      letBindSubterm
      ( \idents ->
          buildCaseTreeFromRows $ rebuildCaseRow idents rowsWithMatch
      )
      (patternSubterms chosenColumn)
      []

  buildCaseBranch
    :: (ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr))
    -> ConvertM BackendExpr
  buildCaseBranch guardExpr = do
    let
      { rowsWithMatch, rowsNoMatch } = decompose chosenColumn rows
      exprOnPatternMiss = buildCaseTreeFromRows rowsNoMatch
      exprOnPatternMatch = foldr
        letBindSubterm
        ( \idents ->
            buildCaseTreeFromRows $ rebuildCaseRow idents rowsWithMatch
        )
        (patternSubterms chosenColumn)
        []
    makeGuard chosenColumn.column guardExpr exprOnPatternMatch exprOnPatternMiss

  letBindSubterm
    :: SubPattern
    -> (Array Level -> ConvertM BackendExpr)
    -> Array Level
    -> ConvertM BackendExpr
  letBindSubterm { accessor } nextCb idents = do
    let parentExpr = make $ Local Nothing chosenColumn.column
    makeLet Nothing (make $ Accessor parentExpr accessor) \tmp ->
      nextCb $ Array.snoc idents tmp

  -- | Rebuilds the case row by doing two things:
  -- | 1. replacing the matched pattern with its subterm patterns
  -- | 2. adding the `vars` exposed by this case row's corresponding binder to the case row's `vars`
  rebuildCaseRow :: Array Level -> Array DecomposedCaseRow -> Array CaseRow
  rebuildCaseRow idents = map \row@{ guardFn, nonMatchesBefore, match, nonMatchesAfter } -> do
    let
      subtermPatterns = case patternPatCase match of
        PatWild -> inlineWildSubterms
        _ -> Array.zipWith convertSubtermToPattern idents $ patternSubterms match
    { guardFn
    , vars: row.vars <> toCaseRowVars match
    , patterns: nonMatchesBefore <> subtermPatterns <> nonMatchesAfter
    }
    where
    convertSubtermToPattern :: Level -> SubPattern -> TopPattern
    convertSubtermToPattern column { pattern } = { column, pattern }

    inlineWildSubterms = idents <#> \column -> { column, pattern: Pattern { vars: Set.empty, patternCase: PatWild, subterms: [] } }

-- | Determines whether a case row's patterns had a match or not.
type DecomposeResult a =
  { nonMatchesBefore :: Array a
  , match :: Maybe { match :: a, nonMatchesAfter :: Array a }
  }

-- | A variant of CaseRow where the `patterns` array
-- | has been decomposed into three parts:
-- |  `nonMatchesBefore <> (Array.cons pattern nonMatchesAfter) == patterns`
-- | The `pattern` will be replaced with its subterm patterns (if any)
-- | before we can recurse.
type DecomposedCaseRow =
  { guardFn :: CaseRowGuardedExpr
  , vars :: SemigroupMap Ident (First Level)
  , nonMatchesBefore :: Array TopPattern
  , match :: TopPattern
  , nonMatchesAfter :: Array TopPattern
  }

decompose :: TopPattern -> Array CaseRow -> { rowsWithMatch :: Array DecomposedCaseRow, rowsNoMatch :: Array CaseRow }
decompose chosenColumn = foldMap \row ->
  case NonEmptyArray.fromArray row.patterns of
    Nothing -> unsafeCrashWith "decompose - nextRow.patterns cannot be empty since the first row contains at least one `PatCtor` patternCase"
    Just neaNextRowPatterns -> do
      let result@{ nonMatchesBefore } = foldl1Array (\l -> mergeResults l <<< checkMatch) checkMatch neaNextRowPatterns
      case result.match of
        Just { match, nonMatchesAfter } ->
          { rowsWithMatch: [ { guardFn: row.guardFn, vars: row.vars, nonMatchesBefore, match, nonMatchesAfter } ]
          , rowsNoMatch: if patternPatCase match == PatWild then [ row ] else []
          }
        Nothing ->
          { rowsWithMatch: []
          , rowsNoMatch: [ row ]
          }
  where
  checkMatch :: TopPattern -> DecomposeResult TopPattern
  checkMatch p
    | p.column == chosenColumn.column
    , patternPatCase p == PatWild || on eq patternPatCase chosenColumn p =
        { nonMatchesBefore: []
        , match: Just
            { match: p
            , nonMatchesAfter: []
            }
        }
    | otherwise =
        { nonMatchesBefore: [ p ], match: Nothing }

  mergeResults
    :: forall a
     . DecomposeResult a
    -> DecomposeResult a
    -> DecomposeResult a
  mergeResults l r = case l.match, r.match of
    Just _, Just _ -> unsafeCrashWith "mergeResults - impossible: cannot match the same column twice in the same row"
    Nothing, Nothing -> r { nonMatchesBefore = l.nonMatchesBefore <> r.nonMatchesBefore }
    Nothing, Just _ -> r { nonMatchesBefore = l.nonMatchesBefore <> r.nonMatchesBefore }
    Just lMatch, Nothing -> l { match = Just $ lMatch { nonMatchesAfter = lMatch.nonMatchesAfter <> r.nonMatchesBefore } }

patternFail :: ConvertM (BackendExpr)
patternFail = make (Fail "Failed pattern match")

makeLet :: Maybe Ident -> ConvertM BackendExpr -> (Level -> ConvertM BackendExpr) -> ConvertM BackendExpr
makeLet id a k = do
  lvl <- currentLevel
  case id of
    Nothing ->
      make $ Let id lvl a (levelUp (k lvl))
    Just ident ->
      make $ Let id lvl a (intro [ ident ] lvl (k lvl))

guardInt :: Int -> ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr)
guardInt n lhs = PrimOp (Op2 (OpIntOrd OpEq) lhs (make (Lit (LitInt n))))

guardNumber :: Number -> ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr)
guardNumber n lhs = PrimOp (Op2 (OpNumberOrd OpEq) lhs (make (Lit (LitNumber n))))

guardString :: String -> ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr)
guardString n lhs = PrimOp (Op2 (OpStringOrd OpEq) lhs (make (Lit (LitString n))))

guardChar :: Char -> ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr)
guardChar n lhs = PrimOp (Op2 (OpCharOrd OpEq) lhs (make (Lit (LitChar n))))

guardBoolean :: Boolean -> ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr)
guardBoolean n lhs = PrimOp (Op2 (OpBooleanOrd OpEq) lhs (make (Lit (LitBoolean n))))

guardArrayLength :: Int -> ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr)
guardArrayLength n lhs = guardInt n (make (PrimOp (Op1 OpArrayLength lhs)))

guardTag :: Qualified Ident -> ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr)
guardTag n lhs = PrimOp (Op1 (OpIsTag n) lhs)

makeGuard
  :: Level
  -> (ConvertM BackendExpr -> BackendSyntax (ConvertM BackendExpr))
  -> ConvertM BackendExpr
  -> ConvertM BackendExpr
  -> ConvertM BackendExpr
makeGuard lvl g inner def =
  make $ Branch (NonEmptyArray.singleton (Pair (make (g (make (Local Nothing lvl)))) inner)) def

makeUncurriedAbs
  :: Array Ident
  -> (Array (Tuple (Maybe Ident) Level) -> ConvertM BackendExpr)
  -> ConvertM BackendExpr
makeUncurriedAbs args cb =
  foldr
    ( \ident next tmps -> do
        lvl <- currentLevel
        intro [ ident ] lvl (next (Array.snoc tmps (Tuple (Just ident) lvl)))
    )
    ( \tmps ->
        make $ UncurriedAbs tmps (cb tmps)
    )
    args
    []

make :: BackendSyntax (ConvertM BackendExpr) -> ConvertM BackendExpr
make a = buildM =<< sequence a

toBackendBinding :: Binding Ann -> ConvertM (Tuple Ident BackendExpr)
toBackendBinding (Binding _ ident expr) = Tuple ident <$> toBackendExpr expr
