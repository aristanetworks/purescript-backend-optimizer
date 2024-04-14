module PureScript.Backend.Optimizer.Semantics where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (class Foldable, and, foldMap, foldl, foldr, or)
import Data.Foldable as Foldable
import Data.Foldable as Tuple
import Data.Int as Int
import Data.Int.Bits (complement, shl, shr, xor, zshr, (.&.), (.|.))
import Data.Lazy (Lazy, defer, force)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, unwrap)
import Data.Number as Number
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Optimizer.Analysis (class HasAnalysis, BackendAnalysis(..), Capture(..), Complexity(..), ResultTerm(..), Usage(..), analysisOf, bound, bump, complex, resultOf, updated, withResult, withRewrite)
import PureScript.Backend.Optimizer.CoreFn (ConstructorType, Ident(..), Literal(..), ModuleName, Prop(..), ProperName, Qualified(..), findProp, propKey, propValue)
import PureScript.Backend.Optimizer.Syntax (class HasSyntax, BackendAccessor(..), BackendEffect, BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..), syntaxOf)
import PureScript.Backend.Optimizer.Utils (foldl1Array, foldr1Array)

type Spine a = Array a

type RecSpine a = NonEmptyArray (Tuple Ident (Lazy a))

data MkFn a
  = MkFnApplied a
  | MkFnNext (Maybe Ident) (a -> MkFn a)

data BackendSemantics
  = SemRef EvalRef (Array ExternSpine) (Lazy BackendSemantics)
  | SemLam (Maybe Ident) (BackendSemantics -> BackendSemantics)
  | SemMkFn (MkFn BackendSemantics)
  | SemMkEffectFn (MkFn BackendSemantics)
  | SemLet (Maybe Ident) BackendSemantics (BackendSemantics -> BackendSemantics)
  | SemLetRec (NonEmptyArray (Tuple Ident (RecSpine BackendSemantics -> BackendSemantics))) (RecSpine BackendSemantics -> BackendSemantics)
  | SemEffectBind (Maybe Ident) BackendSemantics (BackendSemantics -> BackendSemantics)
  | SemEffectPure BackendSemantics
  | SemEffectDefer BackendSemantics
  | SemBranch (NonEmptyArray (SemConditional BackendSemantics)) (Lazy BackendSemantics)
  | SemAssocOp (Either (Qualified Ident) BackendOperator2) (NonEmptyArray BackendSemantics)
  | NeutLocal (Maybe Ident) Level
  | NeutVar (Qualified Ident)
  | NeutStop (Qualified Ident)
  | NeutData (Qualified Ident) ConstructorType ProperName Ident (Array (Tuple String BackendSemantics))
  | NeutCtorDef (Qualified Ident) ConstructorType ProperName Ident (Array String)
  | NeutApp BackendSemantics (Spine BackendSemantics)
  | NeutAccessor BackendSemantics BackendAccessor
  | NeutUpdate BackendSemantics (Array (Prop BackendSemantics))
  | NeutLit (Literal BackendSemantics)
  | NeutFail String
  | NeutUncurriedApp BackendSemantics (Array BackendSemantics)
  | NeutUncurriedEffectApp BackendSemantics (Array BackendSemantics)
  | NeutPrimOp (BackendOperator BackendSemantics)
  | NeutPrimEffect (BackendEffect BackendSemantics)
  | NeutPrimUndefined

data SemConditional a = SemConditional (Lazy a) (Lazy a)

data BackendExpr
  = ExprSyntax BackendAnalysis (BackendSyntax BackendExpr)
  | ExprRewrite BackendAnalysis (BackendRewrite BackendExpr)

instance Eq BackendExpr where
  eq = case _, _ of
    ExprSyntax (BackendAnalysis s1) x, ExprSyntax (BackendAnalysis s2) y ->
      s1.size == s2.size && x == y
    ExprRewrite (BackendAnalysis s1) x, ExprRewrite (BackendAnalysis s2) y ->
      s1.size == s2.size && x == y
    _, _ ->
      false

type LetBindingAssoc a =
  { ident :: Maybe Ident
  , level :: Level
  , binding :: a
  }

type EffectBindingAssoc a =
  { ident :: Maybe Ident
  , level :: Level
  , binding :: a
  , pure :: Boolean
  }

data BackendRewrite a
  = RewriteInline (Maybe Ident) Level a a
  | RewriteUncurry (Maybe Ident) Level (NonEmptyArray (Tuple (Maybe Ident) Level)) a a
  | RewriteStop (Qualified Ident)
  | RewriteUnpackOp (Maybe Ident) Level (UnpackOp a) a
  | RewriteDistBranchesLet (Maybe Ident) Level (NonEmptyArray (Pair a)) a a
  | RewriteDistBranchesOp (NonEmptyArray (Pair a)) a (DistOp a)

derive instance Eq a => Eq (BackendRewrite a)

data UnpackOp a
  = UnpackRecord (Array (Prop a))
  | UnpackUpdate a (Array (Prop a))
  | UnpackArray (Array a)
  | UnpackData (Qualified Ident) ConstructorType ProperName Ident (Array (Tuple String a))

derive instance Eq a => Eq (UnpackOp a)

data DistOp a
  = DistApp (NonEmptyArray a)
  | DistUncurriedApp (Array a)
  | DistAccessor BackendAccessor
  | DistPrimOp1 BackendOperator1
  | DistPrimOp2L BackendOperator2 a
  | DistPrimOp2R a BackendOperator2

derive instance Eq a => Eq (DistOp a)

data ExternImpl
  = ExternExpr (Array (Qualified Ident)) NeutralExpr
  | ExternDict (Array (Qualified Ident)) (Array (Prop (Tuple BackendAnalysis NeutralExpr)))
  | ExternCtor DataTypeMeta ConstructorType ProperName Ident (Array String)

type DataTypeMeta =
  { constructors :: Map Ident CtorMeta
  , size :: Int
  }

type CtorMeta =
  { fields :: Array String
  , tag :: Int
  }

instance HasAnalysis BackendExpr where
  analysisOf = case _ of
    ExprSyntax s _ -> s
    ExprRewrite s _ -> s

instance HasSyntax BackendExpr where
  syntaxOf = case _ of
    ExprSyntax _ s -> Just s
    _ -> Nothing

data LocalBinding a = One a | Group (NonEmptyArray (Tuple Ident (Lazy a)))

data ExternSpine
  = ExternApp (Spine BackendSemantics)
  | ExternUncurriedApp (Spine BackendSemantics)
  | ExternAccessor BackendAccessor
  | ExternPrimOp BackendOperator1

data EvalRef
  = EvalExtern (Qualified Ident)
  | EvalLocal (Maybe Ident) Level

derive instance Eq EvalRef
derive instance Ord EvalRef

data InlineAccessor
  = InlineProp String
  | InlineSpineProp String
  | InlineAt

derive instance Eq InlineAccessor
derive instance Ord InlineAccessor

data InlineDirective
  = InlineDefault
  | InlineNever
  | InlineAlways
  | InlineArity Int

data InlineRef
  = InlineExtern (Qualified Ident)
  | InlineDataType (Qualified ProperName)
  | InlineLocal Level

derive instance Eq InlineRef
derive instance Ord InlineRef

type InlineDirectiveMap = Map InlineRef (Map InlineAccessor InlineDirective)

newtype Env = Env
  { currentModule :: ModuleName
  , evalExternRef :: Env -> Qualified Ident -> Maybe BackendSemantics
  , evalExternSpine :: Env -> Qualified Ident -> Array ExternSpine -> Maybe BackendSemantics
  , locals :: Array (LocalBinding BackendSemantics)
  , directives :: InlineDirectiveMap
  }

derive instance Newtype Env _

lookupLocal :: Env -> Level -> Maybe (LocalBinding BackendSemantics)
lookupLocal (Env { locals }) (Level lvl) = Array.index locals lvl

bindLocal :: Env -> LocalBinding BackendSemantics -> Env
bindLocal (Env env) sem = Env env { locals = Array.snoc env.locals sem }

insertDirective :: InlineRef -> InlineAccessor -> InlineDirective -> InlineDirectiveMap -> InlineDirectiveMap
insertDirective ref acc dir = Map.alter
  case _ of
    Just dirs ->
      Just $ Map.insert acc dir dirs
    Nothing ->
      Just $ Map.singleton acc dir
  ref

addStop :: Env -> InlineRef -> InlineAccessor -> Env
addStop (Env env) ref acc = Env env
  { directives = Map.alter
      case _ of
        Just dirs ->
          Just $ Map.insert acc InlineNever dirs
        _ ->
          Just $ Map.singleton acc InlineNever
      ref
      env.directives
  }

class Eval f where
  eval :: Env -> f -> BackendSemantics

instance Eval f => Eval (BackendSyntax f) where
  eval env@(Env e) = case _ of
    Var qual ->
      case e.evalExternSpine env qual [] of
        Just sem ->
          sem
        Nothing ->
          SemRef (EvalExtern qual) [] $ defer \_ ->
            case e.evalExternRef env qual of
              Just sem ->
                deref sem
              Nothing ->
                NeutVar qual
    Local ident lvl ->
      case lookupLocal env lvl of
        Just (One sem) -> sem
        Just (Group group) | Just sem <- flip Tuple.lookup group =<< ident ->
          force sem
        _ ->
          unsafeCrashWith $ "Unbound local at level " <> show (unwrap lvl)
    App hd tl ->
      evalApp env (eval env hd) (NonEmptyArray.toArray (eval env <$> tl))
    UncurriedApp hd tl ->
      evalUncurriedApp env (eval env hd) (eval env <$> tl)
    UncurriedAbs idents body -> do
      let
        loop env' = case _ of
          List.Nil ->
            MkFnApplied (eval env' body)
          List.Cons a as ->
            MkFnNext a \nextArg ->
              loop (bindLocal env' (One nextArg)) as
      SemMkFn (loop env (Array.toUnfoldable $ map fst idents))
    UncurriedEffectApp hd tl ->
      evalUncurriedEffectApp env (eval env hd) (eval env <$> tl)
    UncurriedEffectAbs idents body -> do
      let
        loop env' = case _ of
          List.Nil ->
            MkFnApplied (eval env' body)
          List.Cons a as ->
            MkFnNext a \nextArg ->
              loop (bindLocal env' (One nextArg)) as
      SemMkEffectFn (loop env (Array.toUnfoldable $ map fst idents))
    Abs idents body ->
      foldr1Array
        (\(Tuple ident _) next env' -> SemLam ident (next <<< bindLocal env' <<< One))
        (\(Tuple ident _) env' -> SemLam ident (flip eval body <<< bindLocal env' <<< One))
        idents
        env
    Let ident _ binding body ->
      makeLet ident (eval env binding) (flip eval body <<< bindLocal env <<< One)
    LetRec _ bindings body -> do
      let bindGroup sem = flip eval sem <<< bindLocal env <<< Group
      SemLetRec (map bindGroup <$> bindings) (bindGroup body)
    EffectBind ident _ binding body ->
      makeEffectBind ident (eval env binding) (flip eval body <<< bindLocal env <<< One)
    EffectPure val ->
      guardFail (eval env val) SemEffectPure
    EffectDefer val ->
      guardFail (eval env val) SemEffectDefer
    Accessor lhs accessor ->
      evalAccessor env (eval env lhs) accessor
    Update lhs updates ->
      evalUpdate (eval env lhs) (map (eval env) <$> updates)
    Branch branches def ->
      evalBranches env (evalPair env <$> branches) (defer \_ -> eval env def)
    PrimOp op ->
      evalPrimOp env (eval env <$> op)
    PrimEffect eff ->
      guardFailOver identity (eval env <$> eff) NeutPrimEffect
    PrimUndefined ->
      NeutPrimUndefined
    Lit lit ->
      guardFailOver identity (eval env <$> lit) NeutLit
    Fail err ->
      NeutFail err
    CtorDef ct ty tag fields ->
      NeutCtorDef (Qualified (Just (unwrap env).currentModule) tag) ct ty tag fields
    CtorSaturated qual ct ty tag fields ->
      guardFailOver snd (map (eval env) <$> fields) $ NeutData qual ct ty tag

instance Eval BackendExpr where
  eval = go
    where
    go env = case _ of
      ExprRewrite _ rewrite ->
        case rewrite of
          RewriteInline _ _ binding body ->
            go (bindLocal env (One (eval env binding))) body
          RewriteUncurry ident _ args binding body ->
            SemLet ident (mkFnFromArgs env (NonEmptyArray.toArray args) binding) \newFn -> do
              eval (bindLocal env (One (mkUncurriedAppRewrite env newFn (NonEmptyArray.length args)))) body
          RewriteStop qual ->
            NeutStop qual
          RewriteUnpackOp _ _ op body ->
            case op of
              UnpackRecord props ->
                foldr
                  ( \(Prop prop expr) next props' ->
                      makeLet Nothing (eval env expr) \val ->
                        next (Array.snoc props' (Prop prop val))
                  )
                  (flip eval body <<< bindLocal env <<< One <<< NeutLit <<< LitRecord)
                  props
                  []
              UnpackUpdate hd props ->
                makeLet Nothing (eval env hd) \hd' ->
                  foldr
                    ( \(Prop prop expr) next props' ->
                        makeLet Nothing (eval env expr) \val ->
                          next (Array.snoc props' (Prop prop val))
                    )
                    (flip eval body <<< bindLocal env <<< One <<< NeutUpdate hd')
                    props
                    []
              UnpackArray exprs ->
                foldr
                  ( \expr next exprs' ->
                      makeLet Nothing (eval env expr) \val ->
                        next (Array.snoc exprs' val)
                  )
                  (flip eval body <<< bindLocal env <<< One <<< NeutLit <<< LitArray)
                  exprs
                  []
              UnpackData qual ct ty tag fields ->
                foldr
                  ( \(Tuple field expr) next props' ->
                      makeLet Nothing (eval env expr) \val ->
                        next (Array.snoc props' (Tuple field val))
                  )
                  (flip eval body <<< bindLocal env <<< One <<< NeutData qual ct ty tag)
                  fields
                  []
          RewriteDistBranchesLet _ _ branches def body ->
            rewriteBranches (flip eval body <<< bindLocal env <<< One)
              $ evalBranches env (evalPair env <$> branches) (defer \_ -> eval env def)
          RewriteDistBranchesOp branches def op ->
            rewriteBranches dist $ evalBranches env (evalPair env <$> branches) (defer \_ -> eval env def)
            where
            dist = case op of
              DistApp spine ->
                flip (evalApp env) (NonEmptyArray.toArray (eval env <$> spine))
              DistUncurriedApp spine ->
                flip (evalUncurriedApp env) (eval env <$> spine)
              DistAccessor acc ->
                flip (evalAccessor env) acc
              DistPrimOp1 op1 ->
                evalPrimOp env <<< Op1 op1
              DistPrimOp2L op2 rhs ->
                evalPrimOp env <<< flip (Op2 op2) (eval env rhs)
              DistPrimOp2R lhs op2 ->
                evalPrimOp env <<< Op2 op2 (eval env lhs)
      ExprSyntax _ expr ->
        eval env expr

instance Eval NeutralExpr where
  eval env (NeutralExpr a) = eval env a

snocApp :: Array ExternSpine -> BackendSemantics -> Array ExternSpine
snocApp prev next = case Array.last prev of
  Just (ExternApp apps) ->
    Array.snoc (Array.dropEnd 1 prev) (ExternApp (Array.snoc apps next))
  _ ->
    Array.snoc prev (ExternApp [ next ])

evalApp :: Env -> BackendSemantics -> Spine BackendSemantics -> BackendSemantics
evalApp env hd spine = go env hd (List.fromFoldable spine)
  where
  go env' = case _, _ of
    _, List.Cons (NeutFail err) _ ->
      NeutFail err
    NeutFail err, _ ->
      NeutFail err
    SemLam _ k, List.Cons arg args ->
      makeLet Nothing arg \nextArg ->
        go env' (k nextArg) args
    SemRef ref sp sem, List.Cons arg args ->
      go env' (evalRef env' ref sp (ExternApp [ arg ]) sem) args
    SemLet ident val k, args ->
      SemLet ident val \nextVal ->
        makeLet Nothing (k nextVal) \nextFn ->
          go (bindLocal (bindLocal env' (One nextVal)) (One nextFn)) nextFn args
    SemLetRec vals k, args ->
      SemLetRec vals \nextVals ->
        makeLet Nothing (k nextVals) \nextFn ->
          go (bindLocal (bindLocal env' (Group nextVals)) (One nextFn)) nextFn args
    fn, List.Nil ->
      fn
    fn, args ->
      NeutApp fn (List.toUnfoldable args)

evalUncurriedApp :: Env -> BackendSemantics -> Spine BackendSemantics -> BackendSemantics
evalUncurriedApp env hd spine = case hd of
  SemMkFn mk ->
    evalUncurriedBeta NeutUncurriedApp mk spine
  SemRef ref sp sem ->
    guardFailOver identity spine \spine' ->
      evalRef env ref sp (ExternUncurriedApp spine') sem
  SemLet ident val k ->
    SemLet ident val \nextVal ->
      makeLet Nothing (k nextVal) \nextFn ->
        evalUncurriedApp (bindLocal (bindLocal env (One nextVal)) (One nextFn)) nextFn spine
  NeutFail err ->
    NeutFail err
  _ ->
    guardFailOver identity spine (NeutUncurriedApp hd)

evalUncurriedEffectApp :: Env -> BackendSemantics -> Spine BackendSemantics -> BackendSemantics
evalUncurriedEffectApp env hd spine = case hd of
  SemMkEffectFn mk ->
    evalUncurriedBeta NeutUncurriedEffectApp mk spine
  SemLet ident val k ->
    SemLet ident val \nextVal ->
      makeLet Nothing (k nextVal) \nextFn ->
        evalUncurriedEffectApp (bindLocal (bindLocal env (One nextVal)) (One nextFn)) nextFn spine
  NeutFail err ->
    NeutFail err
  _ ->
    guardFailOver identity spine (NeutUncurriedEffectApp hd)

evalUncurriedBeta :: (BackendSemantics -> Spine BackendSemantics -> BackendSemantics) -> MkFn BackendSemantics -> Spine BackendSemantics -> BackendSemantics
evalUncurriedBeta fn mk spine = go mk (List.fromFoldable spine)
  where
  go = case _, _ of
    MkFnNext _ _, List.Cons (NeutFail err) _ ->
      NeutFail err
    MkFnNext _ k, List.Cons arg args ->
      makeLet Nothing arg \nextArg ->
        go (k nextArg) args
    MkFnNext _ _, _ ->
      unsafeCrashWith "Uncurried function applied to too few arguments"
    MkFnApplied a, List.Nil ->
      a
    MkFnApplied a, args ->
      fn a (List.toUnfoldable args)

evalSpine :: Env -> BackendSemantics -> Array ExternSpine -> BackendSemantics
evalSpine env = foldl go
  where
  go hd = case _ of
    ExternApp spine ->
      evalApp env hd spine
    ExternUncurriedApp spine ->
      evalUncurriedApp env hd spine
    ExternAccessor accessor ->
      evalAccessor env hd accessor
    ExternPrimOp op1 ->
      evalPrimOp env (Op1 op1 hd)

neutralSpine :: BackendSemantics -> Array ExternSpine -> BackendSemantics
neutralSpine = foldl go
  where
  go hd = case _ of
    ExternApp apps ->
      NeutApp hd apps
    ExternUncurriedApp apps ->
      NeutUncurriedApp hd apps
    ExternAccessor acc ->
      NeutAccessor hd acc
    ExternPrimOp op1 ->
      NeutPrimOp (Op1 op1 hd)

neutralApp :: BackendSemantics -> Spine BackendSemantics -> BackendSemantics
neutralApp hd spine
  | Array.null spine =
      hd
  | otherwise = case hd of
      NeutApp hd' spine' ->
        NeutApp hd' (spine' <> spine)
      _ ->
        NeutApp hd spine

evalAccessor :: Env -> BackendSemantics -> BackendAccessor -> BackendSemantics
evalAccessor env lhs accessor = floatLet lhs case _ of
  SemRef ref spine sem ->
    evalRef env ref spine (ExternAccessor accessor) sem
  NeutLit (LitRecord props)
    | GetProp prop <- accessor
    , Just sem <- Array.findMap (\(Prop p v) -> guard (p == prop) $> v) props ->
        sem
  NeutUpdate rec props
    | GetProp prop <- accessor ->
        case Array.findMap (\(Prop p v) -> guard (p == prop) $> v) props of
          Just sem ->
            sem
          Nothing ->
            evalAccessor env rec accessor
  NeutLit (LitArray values)
    | GetIndex n <- accessor
    , Just sem <- Array.index values n ->
        sem
  NeutData _ _ _ _ fields
    | GetCtorField _ _ _ _ _ n <- accessor
    , Just (Tuple _ sem) <- Array.index fields n ->
        sem
  NeutFail err ->
    NeutFail err
  lhs' ->
    NeutAccessor lhs' accessor

evalUpdate :: BackendSemantics -> Array (Prop BackendSemantics) -> BackendSemantics
evalUpdate lhs props = floatLet lhs case _ of
  NeutLit (LitRecord props') ->
    NeutLit (LitRecord (NonEmptyArray.head <$> Array.groupAllBy (comparing propKey) (props <> props')))
  NeutUpdate r props' ->
    NeutUpdate r (NonEmptyArray.head <$> Array.groupAllBy (comparing propKey) (props <> props'))
  lhs' ->
    NeutUpdate lhs' props

evalBranches :: Env -> NonEmptyArray (SemConditional BackendSemantics) -> Lazy BackendSemantics -> BackendSemantics
evalBranches _ initConds initDef = go [] (NonEmptyArray.toArray initConds) initDef
  where
  go acc conds def = case Array.uncons conds of
    Just { head, tail } ->
      case head of
        SemConditional c t -> case deref (force c) of
          NeutLit (LitBoolean didMatch)
            | didMatch -> go acc [] t
            | otherwise -> go acc tail def
          NeutFail err ->
            go acc [] (defer \_ -> NeutFail err)
          _ ->
            go (Array.snoc acc head) tail def
    Nothing ->
      case NonEmptyArray.fromArray acc of
        Just bs ->
          SemBranch bs def
        Nothing ->
          force def

rewriteBranches :: (BackendSemantics -> BackendSemantics) -> BackendSemantics -> BackendSemantics
rewriteBranches k = go
  where
  go = case _ of
    SemLet a b c ->
      SemLet a b (go <$> c)
    SemLetRec a b ->
      SemLetRec a (go <$> b)
    SemBranch bs def ->
      SemBranch ((\(SemConditional a b) -> SemConditional a (go <$> b)) <$> bs) (go <$> def)
    sem ->
      k sem

evalPair :: forall f. Eval f => Env -> Pair f -> SemConditional BackendSemantics
evalPair env (Pair a b) = SemConditional (defer \_ -> eval env a) (defer \_ -> eval env b)

makeEffectBind :: Maybe Ident -> BackendSemantics -> (BackendSemantics -> BackendSemantics) -> BackendSemantics
makeEffectBind = go
  where
  go ident1 binding1 k1 = case binding1 of
    SemLet ident2 binding2 k2 ->
      makeLet ident2 binding2 \nextBinding2 ->
        makeEffectBind ident1 (k2 nextBinding2) k1
    SemEffectBind ident2 binding2 k2 ->
      go ident2 binding2 \nextBinding2 ->
        makeEffectBind ident1 (k2 nextBinding2) k1
    SemEffectDefer binding2 ->
      SemEffectDefer $ floatLet binding2 \nextBinding2 ->
        makeEffectBind ident1 nextBinding2 k1
    _ ->
      floatLet binding1 \nextBinding2 ->
        SemEffectBind ident1 nextBinding2 k1

makeLet :: Maybe Ident -> BackendSemantics -> (BackendSemantics -> BackendSemantics) -> BackendSemantics
makeLet = floatLetWith go
  where
  go ident binding k = case binding of
    SemRef _ [] _ ->
      k binding
    NeutLocal _ _ ->
      k binding
    NeutStop _ ->
      k binding
    NeutVar _ ->
      k binding
    _ ->
      SemLet ident binding k

floatLet :: BackendSemantics -> (BackendSemantics -> BackendSemantics) -> BackendSemantics
floatLet = floatLetWith (const (#)) Nothing

floatLetWith
  :: (Maybe Ident -> BackendSemantics -> (BackendSemantics -> BackendSemantics) -> BackendSemantics)
  -> Maybe Ident
  -> BackendSemantics
  -> (BackendSemantics -> BackendSemantics)
  -> BackendSemantics
floatLetWith = go
  where
  go f ident1 binding1 k1 = case binding1 of
    SemLet ident2 binding2 k2 ->
      go makeLet ident2 binding2 \nextBinding2 ->
        f ident1 (k2 nextBinding2) k1
    SemLetRec bindings k2 ->
      SemLetRec bindings \nextBindings ->
        makeLet ident1 (k2 nextBindings) k1
    NeutFail _ ->
      binding1
    _ ->
      f ident1 binding1 k1

deref :: BackendSemantics -> BackendSemantics
deref = case _ of
  SemRef _ _ sem ->
    force sem
  sem ->
    sem

evalPrimOp :: Env -> BackendOperator BackendSemantics -> BackendSemantics
evalPrimOp env = case _ of
  Op1 op1 x ->
    case op1, x of
      OpBooleanNot, _
        | NeutLit (LitBoolean bool) <- deref x ->
            liftBoolean (not bool)
      OpBooleanNot, _
        | NeutPrimOp op <- x ->
            evalPrimOpNot op
      OpIntBitNot, _
        | NeutLit (LitInt a) <- deref x ->
            liftInt (complement a)
      OpIsTag a, _
        | NeutData b _ _ _ _ <- deref x ->
            liftBoolean (a == b)
      OpArrayLength, _
        | NeutLit (LitArray arr) <- deref x ->
            liftInt (Array.length arr)
      OpIntNegate, _
        | NeutLit (LitInt a) <- deref x ->
            liftInt (negate a)
      OpNumberNegate, _
        | NeutLit (LitNumber a) <- deref x ->
            liftNumber (negate a)
      _, SemRef ref spine sem ->
        evalRef env ref spine (ExternPrimOp op1) sem
      _, NeutFail err ->
        NeutFail err
      _, _ ->
        floatLet x (NeutPrimOp <<< Op1 op1)
  Op2 op2 x y ->
    case op2 of
      OpBooleanAnd
        | NeutLit (LitBoolean false) <- deref x ->
            x
        | NeutLit (LitBoolean false) <- deref y ->
            y
        | NeutLit (LitBoolean true) <- deref x ->
            y
        | NeutLit (LitBoolean true) <- deref y ->
            x
      OpBooleanOr
        | NeutLit (LitBoolean false) <- deref x ->
            y
        | NeutLit (LitBoolean false) <- deref y ->
            x
        | NeutLit (LitBoolean true) <- deref x ->
            x
        | NeutLit (LitBoolean true) <- deref y ->
            y
      OpBooleanOrd OpEq
        | NeutLit (LitBoolean bool) <- deref x ->
            if bool then y else evalPrimOp env (Op1 OpBooleanNot y)
        | NeutLit (LitBoolean bool) <- deref y ->
            if bool then x else evalPrimOp env (Op1 OpBooleanNot x)
      OpBooleanOrd op
        | NeutLit (LitBoolean a) <- deref x
        , NeutLit (LitBoolean b) <- deref y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpCharOrd op
        | NeutLit (LitChar a) <- deref x
        , NeutLit (LitChar b) <- deref y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpIntBitAnd
        | NeutLit (LitInt a) <- deref x
        , NeutLit (LitInt b) <- deref y ->
            liftInt (a .&. b)
      OpIntBitOr
        | NeutLit (LitInt a) <- deref x
        , NeutLit (LitInt b) <- deref y ->
            liftInt (a .|. b)
      OpIntBitShiftLeft
        | NeutLit (LitInt a) <- deref x
        , NeutLit (LitInt b) <- deref y ->
            liftInt (shl a b)
      OpIntBitShiftRight
        | NeutLit (LitInt a) <- deref x
        , NeutLit (LitInt b) <- deref y ->
            liftInt (shr a b)
      OpIntBitXor
        | NeutLit (LitInt a) <- deref x
        , NeutLit (LitInt b) <- deref y ->
            liftInt (xor a b)
      OpIntBitZeroFillShiftRight
        | NeutLit (LitInt a) <- deref x
        , NeutLit (LitInt b) <- deref y ->
            liftInt (zshr a b)
      OpIntNum OpSubtract
        | NeutLit (LitInt 0) <- deref x ->
            evalPrimOp env (Op1 OpIntNegate y)
      OpIntNum op
        | Just result <- evalPrimOpNumInt op x y ->
            result
      OpIntOrd op
        | NeutLit (LitInt a) <- deref x
        , NeutLit (LitInt b) <- deref y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpNumberNum OpSubtract
        | NeutLit (LitNumber 0.0) <- deref x ->
            evalPrimOp env (Op1 OpNumberNegate y)
      OpNumberNum op
        | Just result <- evalPrimOpNumNumber op x y ->
            result
      OpNumberOrd op
        | NeutLit (LitNumber a) <- deref x
        , NeutLit (LitNumber b) <- deref y ->
            liftBoolean (evalPrimOpOrdNumber op a b)
      OpStringOrd op
        | NeutLit (LitString a) <- deref x
        , NeutLit (LitString b) <- deref y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpStringAppend
        | NeutLit (LitString a) <- x
        , NeutLit (LitString b) <- y ->
            liftString (a <> b)
      OpArrayIndex
        | NeutLit (LitInt n) <- y ->
            evalAccessor env x (GetIndex n)
      OpBooleanAnd -> -- Lazy operator should not be reassociated

        case x, y of
          NeutFail err, _ -> NeutFail err
          _, NeutFail err -> NeutFail err
          _, _ -> NeutPrimOp (Op2 op2 x y)
      OpBooleanOr -> -- Lazy operator should not be reassociated

        case x, y of
          NeutFail err, _ -> NeutFail err
          _, NeutFail err -> NeutFail err
          _, _ -> NeutPrimOp (Op2 op2 x y)
      _ ->
        case x, y of
          NeutFail err, _ -> NeutFail err
          _, NeutFail err -> NeutFail err
          _, _ ->
            floatLet x \x' ->
              floatLet y \y' ->
                if isAssocPrimOp op2 then
                  evalAssocOp env (Right op2) x' y'
                else
                  NeutPrimOp (Op2 op2 x' y')

evalPrimOpOrd :: forall a. Ord a => BackendOperatorOrd -> a -> a -> Boolean
evalPrimOpOrd op x y = case op of
  OpEq -> x == y
  OpNotEq -> x /= y
  OpGt -> x > y
  OpGte -> x >= y
  OpLt -> x < y
  OpLte -> x <= y

-- Duplicate because inlined operators behave differently with NaN.
-- This just ensures we get the same behavior for const eval.
evalPrimOpOrdNumber :: BackendOperatorOrd -> Number -> Number -> Boolean
evalPrimOpOrdNumber op x y = case op of
  OpEq -> x == y
  OpNotEq -> x /= y
  OpGt -> x > y
  OpGte -> x >= y
  OpLt -> x < y
  OpLte -> x <= y

evalPrimOpNumNumber :: BackendOperatorNum -> BackendSemantics -> BackendSemantics -> Maybe BackendSemantics
evalPrimOpNumNumber op x y
  | NeutLit (LitNumber a) <- deref x
  , NeutLit (LitNumber b) <- deref y =
      Just $ liftNumber case op of
        OpAdd -> a + b
        OpMultiply -> a * b
        OpSubtract -> a - b
        OpDivide -> a / b
  | otherwise =
      Nothing

evalPrimOpNumInt :: BackendOperatorNum -> BackendSemantics -> BackendSemantics -> Maybe BackendSemantics
evalPrimOpNumInt op x y
  | NeutLit (LitInt a) <- deref x
  , NeutLit (LitInt b) <- deref y =
      case op of
        OpAdd -> do
          let res = a + b
          if b > 0 && res < a || b < 0 && res > a then Nothing
          else Just $ liftInt res
        OpMultiply -> do
          let res = a * b
          if a /= (res / b) then Nothing
          else Just $ liftInt res
        OpSubtract -> do
          let res = a - b
          if b > 0 && res > a || b < 0 && res < a then Nothing
          else Just $ liftInt res
        OpDivide ->
          Just $ liftInt (a / b)
  | otherwise =
      Nothing

evalPrimOpNot :: BackendOperator BackendSemantics -> BackendSemantics
evalPrimOpNot = case _ of
  Op1 op x ->
    case op of
      OpBooleanNot ->
        x
      _ ->
        liftOp1 OpBooleanNot (liftOp1 op x)
  Op2 op x y ->
    case op of
      OpIntOrd ord ->
        liftOp2 (OpIntOrd (primOpOrdNot ord)) x y
      OpNumberOrd ord ->
        liftOp2 (OpNumberOrd (primOpOrdNot ord)) x y
      OpStringOrd ord ->
        liftOp2 (OpStringOrd (primOpOrdNot ord)) x y
      OpCharOrd ord ->
        liftOp2 (OpCharOrd (primOpOrdNot ord)) x y
      OpBooleanOrd ord ->
        liftOp2 (OpBooleanOrd (primOpOrdNot ord)) x y
      _ ->
        liftOp1 OpBooleanNot (liftOp2 op x y)

primOpOrdNot :: BackendOperatorOrd -> BackendOperatorOrd
primOpOrdNot = case _ of
  OpEq -> OpNotEq
  OpNotEq -> OpEq
  OpLt -> OpGte
  OpLte -> OpGt
  OpGt -> OpLte
  OpGte -> OpLt

isAssocPrimOp :: BackendOperator2 -> Boolean
isAssocPrimOp = case _ of
  OpIntNum OpAdd -> true
  OpIntNum OpMultiply -> true
  OpNumberNum OpAdd -> true
  OpNumberNum OpMultiply -> true
  OpStringAppend -> true
  _ -> false

evalAssocOp :: Env -> Either (Qualified Ident) BackendOperator2 -> BackendSemantics -> BackendSemantics -> BackendSemantics
evalAssocOp env op1 = case _, _ of
  SemAssocOp op2 as, SemAssocOp op3 bs
    | op1 == op2
    , op2 == op3 ->
        case evalAssocOp' env op1 (NonEmptyArray.last as) (NonEmptyArray.head bs) of
          SemAssocOp op4 cs
            | op3 == op4 ->
                SemAssocOp op1 $ NonEmptyArray.prependArray (NonEmptyArray.init as) (NonEmptyArray.appendArray cs (NonEmptyArray.tail bs))
          c ->
            SemAssocOp op1 $ NonEmptyArray.prependArray (NonEmptyArray.init as) (NonEmptyArray.cons' c (NonEmptyArray.tail bs))
  a, SemAssocOp op2 bs
    | op1 == op2 ->
        case evalAssocOp' env op1 a (NonEmptyArray.head bs) of
          SemAssocOp op3 cs
            | op2 == op3 ->
                SemAssocOp op1 $ NonEmptyArray.appendArray cs (NonEmptyArray.tail bs)
          a' ->
            SemAssocOp op1 $ NonEmptyArray.cons' a' (NonEmptyArray.tail bs)
  SemAssocOp op2 as, b
    | op1 == op2 ->
        case evalAssocOp' env op1 (NonEmptyArray.last as) b of
          SemAssocOp op3 cs
            | op2 == op3 ->
                SemAssocOp op1 $ NonEmptyArray.prependArray (NonEmptyArray.init as) cs
          b' ->
            SemAssocOp op1 $ NonEmptyArray.snoc' (NonEmptyArray.init as) b'
  a, b ->
    SemAssocOp op1 $ NonEmptyArray.cons' a [ b ]

evalAssocOp' :: Env -> Either (Qualified Ident) BackendOperator2 -> BackendSemantics -> BackendSemantics -> BackendSemantics
evalAssocOp' env@(Env e) op a b = case op of
  Left qual ->
    case e.evalExternSpine env qual [ ExternApp [ a, b ] ] of
      Just res ->
        res
      Nothing ->
        SemAssocOp op $ NonEmptyArray.cons' a [ b ]
  Right primOp ->
    evalPrimOp env (Op2 primOp a b)

evalRef :: Env -> EvalRef -> Array ExternSpine -> ExternSpine -> Lazy BackendSemantics -> BackendSemantics
evalRef env@(Env e) ref spine last sem = case ref of
  EvalExtern qual
    | Just sem' <- e.evalExternSpine env qual spine' ->
        sem'
  _ ->
    SemRef ref spine' $ defer \_ ->
      deref $ evalRefSpine env ref spine' sem last
  where
  spine' = snocSpine spine last

evalRefSpine :: Env -> EvalRef -> Array ExternSpine -> Lazy BackendSemantics -> ExternSpine -> BackendSemantics
evalRefSpine env ref spine sem = case _ of
  ExternApp _ ->
    neutralSpine (evalEvalRef ref) spine
  ExternUncurriedApp _ ->
    neutralSpine (evalEvalRef ref) spine
  ExternAccessor acc ->
    evalAccessor env (force sem) acc
  ExternPrimOp op ->
    evalPrimOp env (Op1 op (force sem))

evalEvalRef :: EvalRef -> BackendSemantics
evalEvalRef = case _ of
  EvalExtern qual ->
    NeutVar qual
  EvalLocal ident lvl ->
    NeutLocal ident lvl

snocSpine :: Array ExternSpine -> ExternSpine -> Array ExternSpine
snocSpine spine = case _ of
  ExternApp apps ->
    foldl snocApp spine apps
  other ->
    Array.snoc spine other

envForGroup :: Env -> InlineRef -> InlineAccessor -> Array (Qualified Ident) -> Env
envForGroup env ref acc group
  | Array.null group = env
  | otherwise = addStop env ref acc

evalExternFromImpl :: Env -> Qualified Ident -> Tuple BackendAnalysis ExternImpl -> Array ExternSpine -> Maybe BackendSemantics
evalExternFromImpl env@(Env e) qual (Tuple analysis impl) spine = case spine of
  [] ->
    case impl of
      ExternExpr group expr -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup InlineAt of
          Just InlineNever ->
            Just $ NeutStop qual
          Just InlineAlways ->
            Just $ eval (envForGroup env ref InlineAt group) expr
          Just (InlineArity _) ->
            Nothing
          _ ->
            case expr of
              NeutralExpr (Lit lit) | shouldInlineExternLiteral lit ->
                Just $ eval (envForGroup env ref InlineAt group) expr
              _ | shouldInlineExternReference qual analysis expr ->
                Just $ eval (envForGroup env ref InlineAt group) expr
              _ ->
                Nothing
      ExternCtor _ ct ty tag [] ->
        Just $ NeutData qual ct ty tag []
      _ ->
        Nothing
  [ ExternAccessor acc@(GetProp prop) ] ->
    case impl of
      ExternExpr group expr -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup (InlineProp prop) of
          Just InlineNever ->
            Just $ neutralSpine (NeutStop qual) spine
          Just InlineAlways ->
            Just $ evalSpine env (eval (envForGroup env ref (InlineProp prop) group) expr) spine
          _ ->
            Nothing
      ExternDict group props | Just (Tuple analysis' body) <- findProp prop props -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup (InlineProp prop) of
          Just InlineNever ->
            Just $ neutralSpine (NeutStop qual) spine
          Just InlineAlways ->
            Just $ eval (envForGroup env ref (InlineProp prop) group) body
          Just (InlineArity _) ->
            Nothing
          _ | shouldInlineExternAccessor qual analysis' body acc ->
            Just $ eval (envForGroup env ref (InlineProp prop) group) body
          _ ->
            Nothing
      _ ->
        Nothing
  [ ExternAccessor (GetProp prop), ExternApp args ] ->
    case impl of
      ExternExpr group expr -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup (InlineProp prop) of
          Just InlineNever ->
            Just $ neutralSpine (NeutStop qual) spine
          Just InlineAlways ->
            Just $ evalSpine env (eval (envForGroup env ref (InlineProp prop) group) expr) spine
          Just (InlineArity n)
            | Array.length args >= n ->
                Just $ evalSpine env (eval (envForGroup env ref (InlineProp prop) group) expr) spine
            | otherwise ->
                Nothing
          _ ->
            Nothing
      ExternDict group props | Just (Tuple analysis' body) <- findProp prop props -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup (InlineProp prop) of
          Just InlineNever ->
            Just $ neutralSpine (NeutStop qual) spine
          Just InlineAlways ->
            Just $ evalApp env (eval (envForGroup env ref (InlineProp prop) group) body) args
          Just (InlineArity n)
            | Array.length args >= n ->
                Just $ evalApp env (eval (envForGroup env ref (InlineProp prop) group) body) args
            | otherwise ->
                Nothing
          _ | shouldInlineExternApp qual analysis' body args ->
            Just $ evalApp env (eval (envForGroup env ref (InlineProp prop) group) body) args
          _ ->
            Nothing
      _ ->
        Nothing
  [ ExternApp args ] ->
    case impl of
      ExternExpr group expr -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup InlineAt of
          Just InlineNever ->
            Just $ neutralSpine (NeutStop qual) spine
          Just InlineAlways ->
            Just $ evalApp env (eval (envForGroup env ref InlineAt group) expr) args
          Just (InlineArity n)
            | Array.length args >= n ->
                Just $ evalApp env (eval (envForGroup env ref InlineAt group) expr) args
            | otherwise ->
                Nothing
          _ | shouldInlineExternApp qual analysis expr args ->
            Just $ evalApp env (eval (envForGroup env ref InlineAt group) expr) args
          _ ->
            Nothing
      ExternCtor _ ct ty tag fields | Array.length fields == Array.length args ->
        Just $ NeutData qual ct ty tag $ Array.zip fields args
      _ ->
        Nothing
  [ ExternApp _, ExternAccessor (GetProp prop) ] ->
    case impl of
      ExternExpr group fn -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup (InlineSpineProp prop) of
          Just InlineNever ->
            Just $ neutralSpine (NeutStop qual) spine
          Just InlineAlways ->
            Just $ evalSpine env (eval (envForGroup env ref (InlineSpineProp prop) group) fn) spine
          _ ->
            Nothing
      _ ->
        Nothing
  [ ExternApp _, ExternAccessor (GetProp prop), ExternApp args2 ] ->
    case impl of
      ExternExpr group fn -> do
        let ref = InlineExtern qual
        case Map.lookup ref e.directives >>= Map.lookup (InlineSpineProp prop) of
          Just InlineNever ->
            Just $ neutralSpine (NeutStop qual) spine
          Just InlineAlways ->
            Just $ evalSpine env (eval (envForGroup env ref (InlineSpineProp prop) group) fn) spine
          Just (InlineArity n) | Array.length args2 >= n ->
            Just $ evalSpine env (eval (envForGroup env ref (InlineSpineProp prop) group) fn) spine
          _ ->
            Nothing
      _ ->
        Nothing
  _ ->
    Nothing

evalExternRefFromImpl :: Env -> Qualified Ident -> Tuple BackendAnalysis ExternImpl -> BackendSemantics
evalExternRefFromImpl env qual (Tuple _ impl) = case impl of
  ExternExpr group (NeutralExpr expr)
    | isRefExpr expr ->
        eval (envForGroup env (InlineExtern qual) InlineAt group) expr
  ExternDict group props ->
    NeutLit $ LitRecord $ map
      ( \(Prop prop (Tuple _ (NeutralExpr expr))) ->
          Prop prop $ eval (envForGroup env (InlineExtern qual) (InlineProp prop) group) expr
      )
      props
  _ ->
    NeutVar qual

isRefExpr :: forall a. BackendSyntax a -> Boolean
isRefExpr = case _ of
  Var _ -> true
  Lit _ -> true
  CtorSaturated _ _ _ _ _ -> true
  Accessor _ _ -> true
  Update _ _ -> true
  PrimOp _ -> true
  _ -> false

analysisFromDirective :: BackendAnalysis -> InlineDirective -> BackendAnalysis
analysisFromDirective (BackendAnalysis analysis) = case _ of
  InlineAlways ->
    mempty
  InlineNever ->
    BackendAnalysis analysis { complexity = NonTrivial, size = top }
  InlineArity n ->
    BackendAnalysis analysis { args = Array.take n analysis.args }
  InlineDefault ->
    BackendAnalysis analysis

liftBoolean :: Boolean -> BackendSemantics
liftBoolean = NeutLit <<< LitBoolean

liftInt :: Int -> BackendSemantics
liftInt = NeutLit <<< LitInt

liftNumber :: Number -> BackendSemantics
liftNumber = NeutLit <<< LitNumber

liftString :: String -> BackendSemantics
liftString = NeutLit <<< LitString

liftOp1 :: BackendOperator1 -> BackendSemantics -> BackendSemantics
liftOp1 op a = NeutPrimOp (Op1 op a)

liftOp2 :: BackendOperator2 -> BackendSemantics -> BackendSemantics -> BackendSemantics
liftOp2 op a b = NeutPrimOp (Op2 op a b)

caseString :: BackendSemantics -> Maybe String
caseString = case _ of
  NeutLit (LitString a) -> Just a
  _ -> Nothing

caseInt :: BackendSemantics -> Maybe Int
caseInt = case _ of
  NeutLit (LitInt a) -> Just a
  _ -> Nothing

caseNumber :: BackendSemantics -> Maybe Number
caseNumber = case _ of
  NeutLit (LitNumber a) -> Just a
  _ -> Nothing

newtype Ctx = Ctx
  { currentLevel :: Int
  , lookupExtern :: Qualified Ident -> Maybe String -> Maybe BackendAnalysis
  , analyze :: Ctx -> BackendSyntax BackendExpr -> BackendAnalysis
  , effect :: Boolean
  , directives :: InlineDirectiveMap
  }

nextLevel :: Ctx -> Tuple Level Ctx
nextLevel (Ctx ctx) = Tuple (Level ctx.currentLevel) $ Ctx ctx { currentLevel = ctx.currentLevel + 1 }

effectfully :: Ctx -> Ctx
effectfully (Ctx ctx)
  | ctx.effect = Ctx ctx
  | otherwise = Ctx ctx { effect = true }

purely :: Ctx -> Ctx
purely (Ctx ctx)
  | ctx.effect = Ctx ctx { effect = false }
  | otherwise = Ctx ctx

quote :: Ctx -> BackendSemantics -> BackendExpr
quote = go
  where
  go ctx = case _ of
    -- Block constructors
    SemLet ident binding k -> do
      let Tuple level ctx' = nextLevel ctx
      build ctx $ Let ident level (quote (purely ctx) binding) $ quote ctx' $ k $ SemRef (EvalLocal ident level) [] $ defer \_ -> deref binding
    SemLetRec bindings k -> do
      let Tuple level ctx' = nextLevel ctx
      -- We are not currently propagating references
      -- to recursive bindings. The language requires
      -- a runtime check for strictly recursive bindings
      -- which we don't currently implement.
      let neutBindings = (\(Tuple ident _) -> Tuple ident $ defer \_ -> NeutLocal (Just ident) level) <$> bindings
      build ctx $ LetRec level
        (map (\b -> quote (purely ctx') $ b neutBindings) <$> bindings)
        (quote ctx' $ k neutBindings)
    SemEffectBind ident binding k -> do
      let ctx' = effectfully ctx
      let Tuple level ctx'' = nextLevel ctx'
      build ctx $ EffectBind ident level (quote ctx' binding) $ quote ctx'' $ k $ NeutLocal ident level
    SemEffectPure sem ->
      build ctx $ EffectPure (quote (purely ctx) sem)
    SemEffectDefer sem ->
      build ctx $ EffectDefer (quote (effectfully ctx) sem)
    SemBranch branches def -> do
      let ctx' = purely ctx
      let quoteCond (SemConditional a b) = Pair (quote ctx' $ force a) (quote ctx $ force b)
      let branches' = quoteCond <$> branches
      foldr (buildBranchCond ctx) (quote ctx <<< force $ def) branches'

    -- Non-block constructors
    SemRef ref sp _ ->
      case ref of
        EvalExtern qual ->
          go ctx $ neutralSpine (NeutVar qual) sp
        EvalLocal ident lvl ->
          go ctx $ neutralSpine (NeutLocal ident lvl) sp
    SemLam ident k -> do
      let Tuple level ctx' = nextLevel ctx
      build ctx $ Abs (NonEmptyArray.singleton (Tuple ident level)) $ quote (purely ctx') $ k $ NeutLocal ident level
    SemMkFn pro -> do
      let
        loop ctx' idents = case _ of
          MkFnNext ident k -> do
            let Tuple lvl ctx'' = nextLevel ctx'
            loop ctx'' (Array.snoc idents (Tuple ident lvl)) (k (NeutLocal ident lvl))
          MkFnApplied body ->
            build ctx' $ UncurriedAbs idents $ quote (purely ctx') body
      loop ctx [] pro
    SemMkEffectFn pro -> do
      let
        loop ctx' idents = case _ of
          MkFnNext ident k -> do
            let Tuple lvl ctx'' = nextLevel ctx'
            loop ctx'' (Array.snoc idents (Tuple ident lvl)) (k (NeutLocal ident lvl))
          MkFnApplied body ->
            build ctx' $ UncurriedEffectAbs idents $ quote (purely ctx') body
      loop ctx [] pro
    SemAssocOp op spine ->
      foldl1Array
        ( \a b -> case op of
            Left qual ->
              build ctx $ App (build ctx (Var qual)) $ NonEmptyArray.cons' a [ quote ctx b ]
            Right primOp ->
              build ctx $ PrimOp (Op2 primOp a (quote ctx b))
        )
        (quote ctx)
        spine
    NeutLocal ident level ->
      build ctx $ Local ident level
    NeutVar qual ->
      build ctx $ Var qual
    NeutStop qual ->
      buildStop ctx qual
    NeutData qual ct ty tag values ->
      build ctx $ CtorSaturated qual ct ty tag (map (quote ctx) <$> values)
    NeutCtorDef _ ct ty tag fields ->
      build ctx $ CtorDef ct ty tag fields
    NeutUncurriedApp hd spine -> do
      let ctx' = purely ctx
      let hd' = quote ctx' hd
      build ctx $ UncurriedApp hd' (quote ctx' <$> spine)
    NeutUncurriedEffectApp hd spine -> do
      let ctx' = purely ctx
      let hd' = quote ctx' hd
      build ctx $ UncurriedEffectApp hd' (quote ctx' <$> spine)
    NeutApp hd spine -> do
      let ctx' = purely ctx
      let hd' = quote ctx' hd
      case NonEmptyArray.fromArray (quote ctx' <$> spine) of
        Nothing ->
          hd'
        Just args ->
          build ctx $ App hd' args
    NeutAccessor lhs accessor ->
      build ctx $ Accessor (quote ctx lhs) accessor
    NeutUpdate lhs props ->
      build ctx $ Update (quote ctx lhs) (map (quote ctx) <$> props)
    NeutLit lit ->
      build ctx $ Lit (quote ctx <$> lit)
    NeutPrimOp op ->
      build ctx $ PrimOp (quote ctx <$> op)
    NeutPrimEffect eff ->
      build ctx $ PrimEffect (quote (purely ctx) <$> eff)
    NeutPrimUndefined ->
      build ctx PrimUndefined
    NeutFail err ->
      build ctx $ Fail err

build :: Ctx -> BackendSyntax BackendExpr -> BackendExpr
build ctx = case _ of
  App (ExprSyntax _ (App hd tl1)) tl2 ->
    build ctx $ App hd (tl1 <> tl2)
  Abs ids1 (ExprSyntax _ (Abs ids2 body)) ->
    build ctx $ Abs (ids1 <> ids2) body
  Let ident level binding body
    | shouldInlineLet level binding body ->
        rewriteInline ident level binding body
    | Just expr <- shouldUncurryAbs ident level binding body ->
        expr
    | Just expr <- shouldUnpackRecord ident level binding body ->
        expr
    | Just expr <- shouldUnpackUpdate ident level binding body ->
        expr
    | Just expr <- shouldUnpackCtor ident level binding body ->
        expr
    | Just expr <- shouldUnpackArray ident level binding body ->
        expr
    | Just expr <- shouldDistributeBranches ctx ident level binding body ->
        expr
    | Just expr <- shouldEtaReduce level binding body ->
        expr
  App (ExprSyntax analysis (Branch bs def)) tl
    | Just expr' <- shouldDistributeBranchApps analysis bs def tl ->
        expr'
  UncurriedApp (ExprSyntax analysis (Branch bs def)) tl
    | Just expr' <- shouldDistributeBranchUncurriedApps analysis bs def tl ->
        expr'
  Accessor (ExprSyntax analysis (Branch bs def)) acc
    | Just expr' <- shouldDistributeBranchAccessor analysis bs def acc ->
        expr'
  PrimOp (Op1 op1 (ExprSyntax analysis (Branch bs def)))
    | Just expr' <- shouldDistributeBranchPrimOp1 analysis bs def op1 ->
        expr'
  PrimOp (Op2 op2 (ExprSyntax analysis (Branch bs def)) rhs)
    | Just expr' <- shouldDistributeBranchPrimOp2L analysis bs def op2 rhs ->
        expr'
  PrimOp (Op2 op2 lhs (ExprSyntax analysis (Branch bs def)))
    | Just expr' <- shouldDistributeBranchPrimOp2R analysis bs def lhs op2 ->
        expr'
  EffectBind ident level (ExprSyntax _ (EffectPure binding)) body ->
    build ctx $ EffectDefer $ build ctx $ Let ident level binding body
  EffectBind ident level (ExprSyntax _ (EffectDefer binding)) body ->
    build ctx $ EffectBind ident level binding body
  EffectBind ident level binding (ExprSyntax _ (EffectDefer body)) ->
    build ctx $ EffectBind ident level binding body
  EffectBind _ level binding (ExprSyntax _ (EffectPure (ExprSyntax _ (Local _ level2)))) | level == level2 ->
    binding
  EffectDefer expr@(ExprSyntax _ (EffectDefer _)) ->
    expr
  PrimOp (Op1 OpBooleanNot (ExprSyntax _ (PrimOp (Op1 OpBooleanNot expr)))) ->
    expr
  expr ->
    buildDefault ctx expr

buildBranchCond :: Ctx -> Pair BackendExpr -> BackendExpr -> BackendExpr
buildBranchCond ctx pair def
  | Just expr <- simplifyCondIsTag ctx pair def =
      expr
  | Just expr <- simplifyCondBoolean ctx pair def =
      expr
  | Just expr <- simplifyCondLiftAnd ctx pair def =
      expr
  | Just expr <- simplifyCondRedundantElse ctx pair def =
      expr
  | ExprSyntax _ (Branch pairs def') <- def =
      build ctx (Branch (NonEmptyArray.cons pair pairs) def')
  | otherwise =
      build ctx (Branch (NonEmptyArray.singleton pair) def)

simplifyCondIsTag :: Ctx -> Pair BackendExpr -> BackendExpr -> Maybe BackendExpr
simplifyCondIsTag _ = case _, _ of
  Pair (ExprSyntax _ (PrimOp (Op1 (OpIsTag _) x1))) (ExprSyntax _ (Lit (LitBoolean false))), def@(ExprSyntax _ (PrimOp (Op1 (OpIsTag _) x2)))
    | x1 == x2 ->
        Just def
  _, _ ->
    Nothing

simplifyCondBoolean :: Ctx -> Pair BackendExpr -> BackendExpr -> Maybe BackendExpr
simplifyCondBoolean ctx = case _, _ of
  Pair expr body@(ExprSyntax _ (Lit (LitBoolean body'))), ExprSyntax _ (Lit (LitBoolean other))
    | body' == other ->
        Just body
    | body' && not other ->
        Just expr
    | not body' && other ->
        Just $ build ctx $ PrimOp (Op1 OpBooleanNot expr)
  Pair expr (ExprSyntax _ (Lit (LitBoolean true))), other
    | isSimplePredicate other ->
        Just $ build ctx $ PrimOp (Op2 OpBooleanOr expr other)
  Pair expr body, ExprSyntax _ (Lit (LitBoolean false)) ->
    Just $ build ctx $ PrimOp (Op2 OpBooleanAnd expr body)
  _, _ ->
    Nothing

isSimplePredicate :: BackendExpr -> Boolean
isSimplePredicate = case _ of
  ExprSyntax _ expr ->
    case expr of
      Lit _ -> true
      Var _ -> true
      Local _ _ -> true
      PrimOp _ -> true
      _ -> false
  _ ->
    false

simplifyCondRedundantElse :: Ctx -> Pair BackendExpr -> BackendExpr -> Maybe BackendExpr
simplifyCondRedundantElse ctx = case _, _ of
  Pair expr1 body1, ExprSyntax _ (Branch pairs _)
    | Pair (ExprSyntax _ (PrimOp (Op1 OpBooleanNot expr2))) body2 <- NonEmptyArray.head pairs
    , expr1 == expr2 ->
        Just $ buildBranchCond ctx (Pair expr1 body1) body2
  _, _ ->
    Nothing

simplifyCondLiftAnd :: Ctx -> Pair BackendExpr -> BackendExpr -> Maybe BackendExpr
simplifyCondLiftAnd ctx pair def1 = case pair of
  Pair x (ExprSyntax _ (Branch pairs def2))
    | [ Pair y z ] <- NonEmptyArray.toArray pairs
    , def1 == def2 ->
        Just $ buildBranchCond ctx (Pair (build ctx (PrimOp (Op2 OpBooleanAnd x y))) z) def1
  _ ->
    Nothing

buildStop :: Ctx -> Qualified Ident -> BackendExpr
buildStop ctx@(Ctx { analyze }) stop = ExprRewrite (analyze ctx (Var stop)) (RewriteStop stop)

buildDefault :: Ctx -> BackendSyntax BackendExpr -> BackendExpr
buildDefault ctx@(Ctx { analyze }) expr = ExprSyntax (analyze ctx expr) expr

rewriteInline :: Maybe Ident -> Level -> BackendExpr -> BackendExpr -> BackendExpr
rewriteInline ident level binding body = do
  let
    s2 = analysisOf body
    powAnalysis = case Map.lookup level (unwrap s2).usages of
      Just (Usage { total }) ->
        -- TODO: There may be more work to be done here wrt size.
        s2 <> power (analysisOf binding) total
      Nothing ->
        s2
  ExprRewrite (withRewrite (bound level powAnalysis)) $ RewriteInline ident level binding body

isReference :: forall a. BackendSyntax a -> Boolean
isReference = case _ of
  Var _ -> true
  Local _ _ -> true
  _ -> false

shouldEtaReduce :: Level -> BackendExpr -> BackendExpr -> Maybe BackendExpr
shouldEtaReduce level1 binding = case _ of
  ExprSyntax _ (Abs args1 (ExprSyntax _ (App (ExprSyntax _ (Local _ level2)) args2)))
    | level1 == level2
    , NonEmptyArray.length args1 == NonEmptyArray.length args2
    , and $ NonEmptyArray.zipWith isSameArg args1 args2 ->
        Just binding
  _ ->
    Nothing
  where
  isSameArg (Tuple _ l1) = case _ of
    ExprSyntax _ (Local _ l2) -> l1 == l2
    _ -> false

shouldUnpackCtor :: Maybe Ident -> Level -> BackendExpr -> BackendExpr -> Maybe BackendExpr
shouldUnpackCtor ident level a body = do
  let BackendAnalysis s2 = analysisOf body
  case a of
    ExprSyntax _ (CtorSaturated qual ct ty tag fields)
      | Just (Usage us) <- Map.lookup level s2.usages
      , us.total == us.access + us.case -> do
          -- TODO: Not sure what to do about analysis, or if it matters.
          let analysis = foldr (append <<< analysisOf <<< snd) (complex NonTrivial (bound level (BackendAnalysis s2))) fields
          Just $ ExprRewrite (withRewrite analysis) $ RewriteUnpackOp ident level (UnpackData qual ct ty tag fields) body
    _ ->
      Nothing

shouldUnpackRecord :: Maybe Ident -> Level -> BackendExpr -> BackendExpr -> Maybe BackendExpr
shouldUnpackRecord ident level binding body = do
  let BackendAnalysis s2 = analysisOf body
  case binding of
    ExprSyntax _ (Lit (LitRecord props))
      | Just (Usage us) <- Map.lookup level s2.usages
      , us.total == us.access + us.update -> do
          -- TODO: Not sure what to do about analysis, or if it matters.
          let analysis = foldr (append <<< analysisOf <<< propValue) (complex NonTrivial (bound level (BackendAnalysis s2))) props
          Just $ ExprRewrite (withRewrite analysis) $ RewriteUnpackOp ident level (UnpackRecord props) body
    _ ->
      Nothing

shouldUnpackUpdate :: Maybe Ident -> Level -> BackendExpr -> BackendExpr -> Maybe BackendExpr
shouldUnpackUpdate ident level binding body = do
  let BackendAnalysis s2 = analysisOf body
  case binding of
    ExprSyntax _ (Update hd props)
      | Just (Usage us) <- Map.lookup level s2.usages
      , us.total == us.access + us.update -> do
          -- TODO: Not sure what to do about analysis, or if it matters.
          let analysis = updated level $ analysisOf hd <> foldr (append <<< analysisOf <<< propValue) (complex NonTrivial (bound level (BackendAnalysis s2))) props
          Just $ ExprRewrite (withRewrite analysis) $ RewriteUnpackOp ident level (UnpackUpdate hd props) body
    _ ->
      Nothing

shouldUnpackArray :: Maybe Ident -> Level -> BackendExpr -> BackendExpr -> Maybe BackendExpr
shouldUnpackArray ident level binding body = do
  let BackendAnalysis s2 = analysisOf body
  case binding of
    ExprSyntax _ (Lit (LitArray exprs))
      | Just (Usage us) <- Map.lookup level s2.usages
      , us.total == us.access -> do
          -- TODO: Not sure what to do about analysis, or if it matters.
          let analysis = foldr (append <<< analysisOf) (complex NonTrivial (bound level (BackendAnalysis s2))) exprs
          Just $ ExprRewrite (withRewrite analysis) $ RewriteUnpackOp ident level (UnpackArray exprs) body
    _ ->
      Nothing

shouldDistributeBranches :: Ctx -> Maybe Ident -> Level -> BackendExpr -> BackendExpr -> Maybe BackendExpr
shouldDistributeBranches (Ctx ctx) ident level a body =
  case a of
    ExprSyntax (BackendAnalysis s1) (Branch branches def) ->
      inlinePredicate s1 (unwrap (analysisOf body)) branches def
    _ ->
      Nothing
  where
  inlinePredicate s1 s2 branches def
    | Known ks <- s1.result
    , Just (Tuple qi pn) <- Set.findMin ks
    , Just directive <- Map.lookup (InlineDataType (qi $> pn)) ctx.directives >>= Map.lookup InlineAt =
        case directive of
          InlineNever ->
            Nothing
          InlineArity _ ->
            Nothing
          InlineAlways ->
            defaultRewrite branches def
          InlineDefault ->
            defaultPredicate s1 s2 branches def
    | otherwise =
        defaultPredicate s1 s2 branches def

  defaultPredicate s1 s2 branches def
    | isConservativeCaseOfCase s1 s2 || isLiberalCaseOfCase s1 s2 branches =
        defaultRewrite branches def
    | otherwise =
        Nothing

  defaultRewrite branches def = do
    -- TODO: Not sure what to do about analysis, or if it matters.
    let analysis = analysisOf a <> bound level (analysisOf body)
    Just $ ExprRewrite (withRewrite analysis) $ RewriteDistBranchesLet ident level branches def body

  nextBranchConds = case _ of
    ExprSyntax _ (Branch conds _) ->
      Just conds
    ExprSyntax _ (Let _ _ (ExprSyntax _ (Branch conds _)) _) ->
      Just conds
    _ ->
      Nothing

  isConservativeCaseOfCase s1 s2
    | Known ks <- s1.result
    , Just conds <- nextBranchConds body
    , Just (Usage us) <- Map.lookup level s2.usages
    , us.captured <= CaptureBranch
    , us.case == NonEmptyArray.length conds
    , us.total == us.access + us.case =
        true
    | otherwise =
        false

  isLiberalCaseOfCase s1 s2 branches
    -- We cap the size of the continuation in the liberal case to
    -- mitigate code explosion. This size is arbitrary, but we try
    -- to scale based on the number of cases.
    | Known _ <- s1.result
    , s2.size <= Int.floor (256.0 / Number.sqrt (Int.toNumber (NonEmptyArray.length branches)))
    , Just (Usage us) <- Map.lookup level s2.usages
    , us.total == us.access + us.case =
        true
    | otherwise =
        false

shouldDistributeBranchApps :: BackendAnalysis -> NonEmptyArray (Pair BackendExpr) -> BackendExpr -> NonEmptyArray BackendExpr -> Maybe BackendExpr
shouldDistributeBranchApps analysis1 branches def spine =
  if NonEmptyArray.all ((_ <= Deref) <<< _.complexity <<< unwrap <<< analysisOf) spine then do
    -- TODO: Not sure what what to do about analysis, or if it matters.
    let analysis = analysis1 <> foldMap analysisOf spine
    Just $ ExprRewrite (withRewrite analysis) $ RewriteDistBranchesOp branches def (DistApp spine)
  else
    Nothing

shouldDistributeBranchUncurriedApps :: BackendAnalysis -> NonEmptyArray (Pair BackendExpr) -> BackendExpr -> Array BackendExpr -> Maybe BackendExpr
shouldDistributeBranchUncurriedApps analysis1 branches def spine =
  if Array.all ((_ <= Deref) <<< _.complexity <<< unwrap <<< analysisOf) spine then do
    -- TODO: Not sure what what to do about analysis, or if it matters.
    let analysis = analysis1 <> foldMap analysisOf spine
    Just $ ExprRewrite (withRewrite analysis) $ RewriteDistBranchesOp branches def (DistUncurriedApp spine)
  else
    Nothing

shouldDistributeBranchAccessor :: BackendAnalysis -> NonEmptyArray (Pair BackendExpr) -> BackendExpr -> BackendAccessor -> Maybe BackendExpr
shouldDistributeBranchAccessor analysis1 branches def acc = do
  -- TODO: Not sure what what to do about analysis, or if it matters.
  let analysis = bump analysis1
  Just $ ExprRewrite (withRewrite analysis) $ RewriteDistBranchesOp branches def (DistAccessor acc)

shouldDistributeBranchPrimOp1 :: BackendAnalysis -> NonEmptyArray (Pair BackendExpr) -> BackendExpr -> BackendOperator1 -> Maybe BackendExpr
shouldDistributeBranchPrimOp1 analysis1 branches def op = do
  -- TODO: Not sure what what to do about analysis, or if it matters.
  let analysis = bump analysis1
  Just $ ExprRewrite (withRewrite analysis) $ RewriteDistBranchesOp branches def (DistPrimOp1 op)

shouldDistributeBranchPrimOp2L :: BackendAnalysis -> NonEmptyArray (Pair BackendExpr) -> BackendExpr -> BackendOperator2 -> BackendExpr -> Maybe BackendExpr
shouldDistributeBranchPrimOp2L analysis1 branches def op2 rhs =
  if (unwrap (analysisOf rhs)).complexity <= Deref then do
    -- TODO: Not sure what what to do about analysis, or if it matters.
    let analysis = bump $ analysis1 <> analysisOf rhs
    Just $ ExprRewrite (withRewrite analysis) $ RewriteDistBranchesOp branches def (DistPrimOp2L op2 rhs)
  else
    Nothing

shouldDistributeBranchPrimOp2R :: BackendAnalysis -> NonEmptyArray (Pair BackendExpr) -> BackendExpr -> BackendExpr -> BackendOperator2 -> Maybe BackendExpr
shouldDistributeBranchPrimOp2R analysis1 branches def lhs op2 =
  if (unwrap (analysisOf lhs)).complexity <= Deref then do
    -- TODO: Not sure what what to do about analysis, or if it matters.
    let analysis = bump $ analysis1 <> analysisOf lhs
    Just $ ExprRewrite (withRewrite analysis) $ RewriteDistBranchesOp branches def (DistPrimOp2R lhs op2)
  else
    Nothing

shouldUncurryAbs :: Maybe Ident -> Level -> BackendExpr -> BackendExpr -> Maybe BackendExpr
shouldUncurryAbs ident level a b = do
  let BackendAnalysis s2 = analysisOf b
  case a of
    ExprSyntax _ (Abs args fn)
      | Just (Usage u) <- Map.lookup level s2.usages
      , [ n ] <- Set.toUnfoldable u.arities
      , n == NonEmptyArray.length args -> do
          let
            analysis =
              withResult (resultOf b)
                $ bump
                $ complex NonTrivial
                $ analysisOf a <> bound level (analysisOf b)
          Just $ ExprRewrite (withRewrite analysis) $ RewriteUncurry ident level args fn b
    _ ->
      Nothing

shouldInlineLet :: Level -> BackendExpr -> BackendExpr -> Boolean
shouldInlineLet level a b = do
  let BackendAnalysis s1 = analysisOf a
  let BackendAnalysis s2 = analysisOf b
  case Map.lookup level s2.usages of
    Nothing ->
      true
    Just (Usage { captured, total, call }) ->
      (s1.complexity == Trivial)
        || (captured == CaptureNone && total == 1)
        || (captured <= CaptureBranch && s1.complexity <= Deref && s1.size < 5)
        || (s1.complexity == Deref && call == total)
        || (s1.complexity == KnownSize && total == 1)
        || (isAbs a && (total == 1 || Map.isEmpty s1.usages || s1.size < 16))
        || (isKnownEffect a && total == 1)

shouldInlineExternReference :: Qualified Ident -> BackendAnalysis -> NeutralExpr -> Boolean
shouldInlineExternReference _ (BackendAnalysis s) _ =
  s.complexity <= Deref && s.size < 16

shouldInlineExternApp :: Qualified Ident -> BackendAnalysis -> NeutralExpr -> Spine BackendSemantics -> Boolean
shouldInlineExternApp _ (BackendAnalysis s) _ args =
  (s.complexity <= Deref && s.size < 16)
    || (Map.isEmpty s.usages && not s.externs && s.size < 64)
    || (delayed && Array.length s.args <= Array.length args && s.size < 16)
    || (delayed && or (Array.zipWith shouldInlineExternAppArg s.args args) && s.size < 16)
  where
  delayed = Array.length s.args > 0

shouldInlineExternAppArg :: Usage -> BackendSemantics -> Boolean
shouldInlineExternAppArg (Usage u) = case _ of
  SemLam _ _ -> u.captured <= CaptureBranch && u.total > 0 && u.call == u.total
  _ -> false

shouldInlineExternAccessor :: Qualified Ident -> BackendAnalysis -> NeutralExpr -> BackendAccessor -> Boolean
shouldInlineExternAccessor _ (BackendAnalysis s) _ _ =
  s.complexity <= Deref && s.size < 16

shouldInlineExternLiteral :: Literal NeutralExpr -> Boolean
shouldInlineExternLiteral = case _ of
  LitInt _ -> true
  LitNumber _ -> true
  LitString s -> String.length s <= 32
  LitChar _ -> true
  LitBoolean _ -> true
  LitArray a -> Array.null a
  LitRecord r -> Array.null r

isAbs :: BackendExpr -> Boolean
isAbs = syntaxOf >>> case _ of
  Just (Abs _ _) -> true
  Just (UncurriedAbs _ _) -> true
  Just (UncurriedEffectAbs _ _) -> true
  Just (EffectDefer _) -> true
  _ -> false

isKnownEffect :: BackendExpr -> Boolean
isKnownEffect = syntaxOf >>> case _ of
  Just (PrimEffect _) -> true
  Just (UncurriedEffectApp _ _) -> true
  Just (EffectBind _ _ _ _) -> true
  Just (EffectDefer _) -> true
  _ -> false

newtype NeutralExpr = NeutralExpr (BackendSyntax NeutralExpr)

derive instance Newtype NeutralExpr _

optimize :: Boolean -> Ctx -> Env -> Qualified Ident -> Int -> BackendExpr -> Tuple (Array BackendExpr) BackendExpr
optimize traceSteps ctx env (Qualified mn (Ident id)) initN originalExpr =
  go (if traceSteps then pure originalExpr else List.Nil) initN originalExpr
  where
  go steps n expr1 = do
    let Tuple rewrite expr2 = goStep n expr1
    let newSteps = if traceSteps then List.Cons expr2 steps else steps
    if rewrite then
      go newSteps (n - 1) expr2
    else
      Tuple (Array.reverse (List.toUnfoldable newSteps)) expr2

  goStep :: Int -> BackendExpr -> Tuple Boolean BackendExpr
  goStep n expr1
    | n == 0 = do
        let name = foldMap ((_ <> ".") <<< unwrap) mn <> id
        unsafeCrashWith $ name <> ": Possible infinite optimization loop."
    | otherwise = do
        let expr2 = quote ctx (eval env expr1)
        let BackendAnalysis { rewrite } = analysisOf expr2
        Tuple rewrite expr2

freeze :: BackendExpr -> Tuple BackendAnalysis NeutralExpr
freeze init = Tuple (analysisOf init) $ foldBackendExpr NeutralExpr (\_ neutExpr -> neutExpr) init

foldBackendExpr :: forall a. (BackendSyntax a -> a) -> (BackendRewrite BackendExpr -> a -> a) -> BackendExpr -> a
foldBackendExpr foldSyntax foldRewrite = go
  where
  go = case _ of
    ExprSyntax _ expr ->
      foldSyntax $ go <$> expr
    ExprRewrite _ rewrite -> foldRewrite rewrite
      case rewrite of
        RewriteInline ident level binding body ->
          foldSyntax $ Let ident level (go binding) (go body)
        RewriteUncurry ident level args binding body ->
          foldSyntax $ Let ident level (foldSyntax (Abs args (go binding))) (go body)
        RewriteStop qual ->
          foldSyntax $ Var qual
        RewriteUnpackOp ident level op body ->
          case op of
            UnpackRecord props ->
              foldSyntax $ Let ident level (foldSyntax (Lit (LitRecord (map go <$> props)))) (go body)
            UnpackUpdate hd props ->
              foldSyntax $ Let ident level (foldSyntax (Update (go hd) (map go <$> props))) (go body)
            UnpackArray exprs ->
              foldSyntax $ Let ident level (foldSyntax (Lit (LitArray (go <$> exprs)))) (go body)
            UnpackData qual ct ty tag values ->
              foldSyntax $ Let ident level (foldSyntax (CtorSaturated qual ct ty tag (map go <$> values))) (go body)
        RewriteDistBranchesLet ident level branches def body ->
          foldSyntax $ Let ident level (foldSyntax (Branch (map go <$> branches) (go def))) (go body)
        RewriteDistBranchesOp branches def op -> do
          let branches' = foldSyntax $ Branch (map go <$> branches) (go def)
          case op of
            DistApp spine ->
              foldSyntax $ App branches' (go <$> spine)
            DistUncurriedApp spine ->
              foldSyntax $ UncurriedApp branches' (go <$> spine)
            DistAccessor acc ->
              foldSyntax $ Accessor branches' acc
            DistPrimOp1 op1 ->
              foldSyntax $ PrimOp $ Op1 op1 branches'
            DistPrimOp2L op2 rhs ->
              foldSyntax $ PrimOp $ Op2 op2 branches' (go rhs)
            DistPrimOp2R lhs op2 ->
              foldSyntax $ PrimOp $ Op2 op2 (go lhs) branches'

evalMkFn :: Env -> Int -> BackendSemantics -> MkFn BackendSemantics
evalMkFn env n sem
  | n == 0 = MkFnApplied sem
  | otherwise =
      case sem of
        SemLam ident k -> do
          MkFnNext ident (evalMkFn env (n - 1) <<< k)
        _ ->
          MkFnNext Nothing \nextArg -> do
            let env' = bindLocal env (One nextArg)
            evalMkFn env' (n - 1) (evalApp env' sem [ nextArg ])

mkUncurriedAppRewrite :: Env -> BackendSemantics -> Int -> BackendSemantics
mkUncurriedAppRewrite env hd = go []
  where
  go acc n
    | n == 0 = evalUncurriedApp env hd acc
    | otherwise =
        SemLam Nothing \arg ->
          go (Array.snoc acc arg) (n - 1)

mkFnFromArgs :: forall f. Eval f => Env -> Array (Tuple (Maybe Ident) Level) -> f -> BackendSemantics
mkFnFromArgs env args body =
  SemMkFn $ foldr
    ( \(Tuple ident _) next env' ->
        MkFnNext ident (next <<< bindLocal env' <<< One)
    )
    (MkFnApplied <<< flip eval body)
    args
    env

guardFail :: BackendSemantics -> (BackendSemantics -> BackendSemantics) -> BackendSemantics
guardFail sem k = case sem of
  NeutFail err -> NeutFail err
  _ -> k sem

guardFailOver :: forall f a. Foldable f => (a -> BackendSemantics) -> f a -> (f a -> BackendSemantics) -> BackendSemantics
guardFailOver f as k =
  case Foldable.findMap (toFail <<< f) as of
    Just err -> err
    Nothing -> k as
  where
  toFail expr = case expr of
    NeutFail _ -> Just expr
    _ -> Nothing
