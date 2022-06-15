module PureScript.Backend.Semantics where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap, foldl)
import Data.Foldable as Tuple
import Data.Int.Bits (complement, shl, shr, xor, zshr, (.&.), (.|.))
import Data.Lazy (Lazy, defer, force)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (power)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.Tuple (Tuple(..), fst, uncurry)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.Backend.Analysis (class HasAnalysis, BackendAnalysis(..), Complexity(..), Usage(..), analysisOf, analyze, bound, withRewrite)
import PureScript.Backend.Syntax (class HasSyntax, BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..), syntaxOf)
import PureScript.CoreFn (Ident, Literal(..), ModuleName, Prop(..), Qualified, findProp, propKey)

type Spine a = Array a

type RecSpine a = Array (Tuple Ident (Lazy a))

data MkFn a
  = MkFnApplied a
  | MkFnNext (Maybe Ident) (a -> MkFn a)

data BackendSemantics
  = SemExtern (Qualified Ident) (Array ExternSpine) (Lazy BackendSemantics)
  | SemLam (Maybe Ident) (BackendSemantics -> BackendSemantics)
  | SemMkFn (MkFn BackendSemantics)
  | SemMkEffectFn (MkFn BackendSemantics)
  | SemLet (Maybe Ident) BackendSemantics (BackendSemantics -> BackendSemantics)
  | SemLetRec (Array (Tuple Ident (RecSpine BackendSemantics -> BackendSemantics))) (RecSpine BackendSemantics -> BackendSemantics)
  | SemEffectBind (Maybe Ident) BackendSemantics (BackendSemantics -> BackendSemantics)
  | SemEffectPure BackendSemantics
  | SemBranch (Array (Pair (Lazy BackendSemantics))) (Maybe (Lazy BackendSemantics))
  | SemBranchTry BackendSemantics (Array (Pair (Lazy BackendSemantics))) (Maybe (Lazy BackendSemantics))
  | SemAccessor BackendSemantics BackendAccessor
  | SemUpdate BackendSemantics (Array (Prop BackendSemantics))
  | SemNeutral BackendNeutral

data BackendNeutral
  = NeutLocal (Maybe Ident) Level
  | NeutVar (Qualified Ident)
  | NeutData (Qualified Ident) Ident (Array (Tuple Ident BackendSemantics))
  | NeutCtorDef Ident (Array Ident)
  | NeutApp BackendSemantics (Spine BackendSemantics)
  | NeutAccessor BackendSemantics BackendAccessor
  | NeutUpdate BackendSemantics (Array (Prop BackendSemantics))
  | NeutLit (Literal BackendSemantics)
  | NeutFail String
  | NeutUncurriedApp BackendSemantics (Array BackendSemantics)
  | NeutUncurriedEffectApp BackendSemantics (Array BackendSemantics)
  | NeutPrimOp (BackendOperator BackendSemantics)

data BackendExpr
  = ExprSyntax BackendAnalysis (BackendSyntax BackendExpr)
  | ExprRewrite BackendAnalysis BackendRewrite

type LetBindingAssoc a =
  { ident :: Maybe Ident
  , level :: Level
  , binding :: a
  }

data BackendRewrite
  = RewriteInline (Maybe Ident) Level BackendExpr BackendExpr
  | RewriteLetAssoc (Array (LetBindingAssoc BackendExpr)) BackendExpr

data Impl
  = ImplExpr NeutralExpr
  | ImplRec (Array (Qualified Ident)) NeutralExpr
  | ImplDict (Array (Prop (Tuple BackendAnalysis NeutralExpr)))
  | ImplCtor Ident (Array Ident)

instance HasAnalysis BackendExpr where
  analysisOf = case _ of
    ExprSyntax s _ -> s
    ExprRewrite s _ -> s

instance HasSyntax BackendExpr where
  syntaxOf = case _ of
    ExprSyntax _ s -> Just s
    _ -> Nothing

data LocalBinding a = One a | Group (Array (Tuple Ident (Lazy a)))

data ExternSpine
  = ExternApp (Spine BackendSemantics)
  | ExternAccessor BackendAccessor

newtype Env = Env
  { currentModule :: ModuleName
  , evalExtern :: Env -> Qualified Ident -> Array ExternSpine -> Maybe BackendSemantics
  , locals :: Array (LocalBinding BackendSemantics)
  }

derive instance Newtype Env _

lookupLocal :: Env -> Level -> Maybe (LocalBinding BackendSemantics)
lookupLocal (Env { locals }) (Level lvl) = Array.index locals lvl

bindLocal :: Env -> LocalBinding BackendSemantics -> Env
bindLocal (Env env) sem = Env env { locals = Array.snoc env.locals sem }

class Eval f where
  eval :: Env -> f -> BackendSemantics

instance Eval f => Eval (BackendSyntax f) where
  eval env = case _ of
    Var qual ->
      case evalExtern env qual [] of
        Just sem -> sem
        Nothing ->
          SemExtern qual [] (defer \_ -> SemNeutral (NeutVar qual))
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
      SemNeutral (NeutUncurriedApp (eval env hd) (eval env <$> tl))
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
      SemNeutral (NeutUncurriedEffectApp (eval env hd) (eval env <$> tl))
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
      SemLet ident (eval env binding) (flip eval body <<< bindLocal env <<< One)
    LetRec _ bindings body -> do
      let bindGroup sem = flip eval sem <<< bindLocal env <<< Group
      SemLetRec (map bindGroup <$> bindings) (bindGroup body)
    EffectBind ident _ binding body ->
      SemEffectBind ident (eval env binding) (flip eval body <<< bindLocal env <<< One)
    EffectPure val ->
      SemEffectPure (eval env val)
    Accessor lhs accessor ->
      evalAccessor env (eval env lhs) accessor
    Update lhs updates ->
      evalUpdate env (eval env lhs) (map (eval env) <$> updates)
    Branch branches def ->
      evalBranches (map (\b -> defer \_ -> eval env b) <$> branches) ((\b -> defer \_ -> eval env b) <$> def)
    PrimOp op ->
      evalPrimOp env (eval env <$> op)
    Lit lit ->
      SemNeutral (NeutLit (eval env <$> lit))
    Fail err ->
      SemNeutral (NeutFail err)
    CtorDef tag fields ->
      SemNeutral (NeutCtorDef tag fields)
    CtorSaturated qual tag fields ->
      SemNeutral (NeutData qual tag (map (eval env) <$> fields))

instance Eval BackendExpr where
  eval = go
    where
    go env = case _ of
      ExprRewrite _ rewrite ->
        case rewrite of
          RewriteInline _ _ binding body ->
            go (bindLocal env (One (eval env binding))) body
          RewriteLetAssoc bindings body -> do
            let
              goBinding env' = case _ of
                List.Nil ->
                  eval env' body
                List.Cons b bs ->
                  SemLet b.ident (eval env' b.binding) \nextBinding ->
                    goBinding (bindLocal env (One nextBinding)) bs
            goBinding env (List.fromFoldable bindings)
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
evalApp env hd spine
  | Array.null spine = hd
  | otherwise = go hd (List.fromFoldable spine)
      where
      go = case _, _ of
        SemLam _ k, List.Cons arg args ->
          SemLet Nothing arg \nextArg ->
            go (k nextArg) args
        SemExtern qual sp _, List.Cons arg args -> do
          let sp' = snocApp sp arg
          case evalExtern env qual sp' of
            Just sem ->
              go sem args
            Nothing ->
              go (SemExtern qual sp' (defer \_ -> neutralSpine qual sp')) args
        -- TODO: Only reassoc the head
        SemLet ident val k, args ->
          SemLet ident val \nextVal ->
            SemLet Nothing (k nextVal) \nextFn ->
              evalApp (bindLocal (bindLocal env (One nextVal)) (One nextFn)) nextFn (List.toUnfoldable args)
        SemNeutral neut, List.Nil ->
          SemNeutral neut
        SemNeutral neut, args ->
          SemNeutral (neutralApp neut (List.toUnfoldable args))
        fn, List.Nil ->
          fn
        fn, args ->
          SemNeutral $ NeutApp fn (List.toUnfoldable args)

evalSpine :: Env -> BackendSemantics -> Array ExternSpine -> BackendSemantics
evalSpine env = foldl go
  where
  go hd = case _ of
    ExternApp spine ->
      evalApp env hd spine
    ExternAccessor accessor ->
      evalAccessor env hd accessor

neutralSpine :: Qualified Ident -> Array ExternSpine -> BackendSemantics
neutralSpine qual = foldl go (SemNeutral (NeutVar qual))
  where
  go hd = case _ of
    ExternApp apps ->
      SemNeutral (NeutApp hd apps)
    ExternAccessor acc ->
      SemNeutral (NeutAccessor hd acc)

neutralApp :: BackendNeutral -> Spine BackendSemantics -> BackendNeutral
neutralApp hd spine
  | Array.null spine =
      hd
  | otherwise = case hd of
      NeutApp hd' spine' ->
        NeutApp hd' (spine' <> spine)
      _ ->
        NeutApp (SemNeutral hd) spine

evalAccessor :: Env -> BackendSemantics -> BackendAccessor -> BackendSemantics
evalAccessor initEnv initLhs accessor =
  evalAssocLet initEnv initLhs \env lhs -> case lhs of
    SemExtern qual spine _ -> do
      let spine' = Array.snoc spine (ExternAccessor accessor)
      case evalExtern env qual spine' of
        Just sem ->
          sem
        Nothing ->
          SemExtern qual spine' (defer \_ -> neutralSpine qual spine')
    SemNeutral (NeutLit (LitRecord props))
      | GetProp prop <- accessor
      , Just sem <- Array.findMap (\(Prop p v) -> guard (p == prop) $> v) props ->
          sem
    SemNeutral (NeutLit (LitArray values))
      | GetIndex n <- accessor
      , Just sem <- Array.index values n ->
          sem
    SemNeutral (NeutData _ _ fields)
      | GetOffset n <- accessor
      , Just (Tuple _ sem) <- Array.index fields n ->
          sem
    _ ->
      SemNeutral (NeutAccessor lhs accessor)

evalUpdate :: Env -> BackendSemantics -> Array (Prop BackendSemantics) -> BackendSemantics
evalUpdate initEnv initLhs props =
  evalAssocLet initEnv initLhs \_ lhs -> case lhs of
    SemNeutral (NeutLit (LitRecord props')) ->
      SemNeutral (NeutLit (LitRecord (NonEmptyArray.head <$> Array.groupAllBy (comparing propKey) (props <> props'))))
    _ ->
      SemNeutral (NeutUpdate lhs props)

evalBranches :: Array (Pair (Lazy BackendSemantics)) -> Maybe (Lazy BackendSemantics) -> BackendSemantics
evalBranches initBranches initDef = go [] initBranches initDef
  where
  go acc branches def = case Array.uncons branches of
    Just { head: Pair pred body, tail } ->
      case force pred of
        SemNeutral (NeutLit (LitBoolean didMatch))
          | didMatch ->
              case force body of
                SemBranch branches' (Just def') ->
                  go acc branches' (Just def')
                SemBranch branches' _ ->
                  go acc (tail <> branches') def
                other ->
                  SemBranch acc $ Just $ defer \_ -> SemBranchTry other branches def
          | otherwise ->
              go acc tail def
        _ ->
          go (Array.snoc acc (Pair pred body)) tail def
    Nothing ->
      if Array.null acc then
        case def of
          Just sem ->
            force sem
          Nothing ->
            SemBranch initBranches Nothing
      else
        SemBranch acc def

evalAssocLet :: Env -> BackendSemantics -> (Env -> BackendSemantics -> BackendSemantics) -> BackendSemantics
evalAssocLet env sem go = case sem of
  SemLet ident val k ->
    SemLet ident val \nextVal1 ->
      SemLet Nothing (k nextVal1) \nextVal2 ->
        go (bindLocal (bindLocal env (One nextVal1)) (One nextVal2)) nextVal2
  _ ->
    go env sem

evalAssocLet2
  :: Env
  -> BackendSemantics
  -> BackendSemantics
  -> (Env -> BackendSemantics -> BackendSemantics -> BackendSemantics)
  -> BackendSemantics
evalAssocLet2 env sem1 sem2 go =
  evalAssocLet env sem1 \env' sem1' ->
    evalAssocLet env' sem2 \env'' sem2' ->
      go env'' sem1' sem2'

-- TODO: Check for overflow in Int ops since backends may not handle it the
-- same was as the JS backend.
evalPrimOp :: Env -> BackendOperator BackendSemantics -> BackendSemantics
evalPrimOp env = case _ of
  Op1 op1 x ->
    case op1, x of
      OpBooleanNot, SemNeutral (NeutLit (LitBoolean bool)) ->
        liftBoolean (not bool)
      OpIntBitNot, SemNeutral (NeutLit (LitInt a)) ->
        liftInt (complement a)
      OpIsTag a, SemNeutral (NeutData b _ _) ->
        liftBoolean (a == b)
      OpArrayLength, SemNeutral (NeutLit (LitArray arr)) ->
        liftInt (Array.length arr)
      _, _ ->
        evalAssocLet env x \_ x'  ->
          SemNeutral (NeutPrimOp (Op1 op1 x'))
  Op2 op2 x y ->
    case op2 of
      OpBooleanAnd
        | SemNeutral (NeutLit (LitBoolean false)) <- x ->
            x
        | SemNeutral (NeutLit (LitBoolean false)) <- y ->
            y
      OpBooleanOr
        | SemNeutral (NeutLit (LitBoolean false)) <- x ->
            y
        | SemNeutral (NeutLit (LitBoolean false)) <- y ->
            x
      OpBooleanOrd OpEq
        | SemNeutral (NeutLit (LitBoolean bool)) <- x ->
            if bool then y else evalPrimOp env (Op1 OpBooleanNot y)
        | SemNeutral (NeutLit (LitBoolean bool)) <- y ->
            if bool then x else evalPrimOp env (Op1 OpBooleanNot x)
      OpBooleanOrd op
        | SemNeutral (NeutLit (LitBoolean a)) <- x
        , SemNeutral (NeutLit (LitBoolean b)) <- y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpCharOrd op
        | SemNeutral (NeutLit (LitChar a)) <- x
        , SemNeutral (NeutLit (LitChar b)) <- y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpIntBitAnd
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftInt (a .&. b)
      OpIntBitOr
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftInt (a .|. b)
      OpIntBitShiftLeft
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftInt (shl a b)
      OpIntBitShiftRight
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftInt (shr a b)
      OpIntBitXor
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftInt (xor a b)
      OpIntBitZeroFillShiftRight
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftInt (zshr a b)
      OpIntNum op
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftInt (evalPrimOpNum op a b)
      OpIntOrd op
        | SemNeutral (NeutLit (LitInt a)) <- x
        , SemNeutral (NeutLit (LitInt b)) <- y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpNumberNum op
        | SemNeutral (NeutLit (LitNumber a)) <- x
        , SemNeutral (NeutLit (LitNumber b)) <- y ->
            liftNumber (evalPrimOpNum op a b)
      OpNumberOrd op
        | SemNeutral (NeutLit (LitNumber a)) <- x
        , SemNeutral (NeutLit (LitNumber b)) <- y ->
            liftBoolean (evalPrimOpOrd op a b)
      OpStringAppend
        | SemNeutral (NeutLit (LitString a)) <- x
        , SemNeutral (NeutLit (LitString b)) <- y ->
            liftString (a <> b)
      OpStringOrd op
        | SemNeutral (NeutLit (LitString a)) <- x
        , SemNeutral (NeutLit (LitString b)) <- y ->
            liftBoolean (evalPrimOpOrd op a b)
      _ ->
        evalAssocLet2 env x y \_ x' y' ->
          SemNeutral (NeutPrimOp (Op2 op2 x' y'))

evalPrimOpOrd :: forall a. Ord a => BackendOperatorOrd -> a -> a -> Boolean
evalPrimOpOrd op x y = case op of
  OpEq -> x == y
  OpGt -> x > y
  OpGte -> x >= y
  OpLt -> x < y
  OpLte -> x <= y

evalPrimOpNum :: forall a. EuclideanRing a => BackendOperatorNum -> a -> a -> a
evalPrimOpNum op x y = case op of
  OpAdd -> x + y
  OpDivide -> x / y
  OpMultiply -> x * y
  OpSubtract -> x - y

evalExtern :: Env -> Qualified Ident -> Array ExternSpine -> Maybe BackendSemantics
evalExtern env@(Env e) = e.evalExtern env

evalExternFromImpl :: Env -> Qualified Ident -> Tuple BackendAnalysis Impl -> Array ExternSpine -> Maybe BackendSemantics
evalExternFromImpl env qual (Tuple analysis impl) spine = case impl of
  ImplExpr expr ->
    case expr, spine of
      NeutralExpr (Var _), [] ->
        Just $ eval env expr
      NeutralExpr (Lit lit), [] | shouldInlineExternLiteral lit ->
        Just $ eval env expr
      NeutralExpr (Lit (LitRecord props)), [ ExternAccessor (GetProp prop) ] ->
        eval env <$> findProp prop props
      body, [ ExternApp args ] | shouldInlineExternApp qual analysis body args ->
        Just $ evalApp env (eval env body) args
      _, _ ->
        Nothing
  ImplCtor tag fields ->
    case fields, spine of
      [], [] ->
        Just $ SemNeutral $ NeutData qual tag []
      _, [ ExternApp args ] | Array.length fields == Array.length args ->
        Just $ SemNeutral $ NeutData qual tag $ Array.zip fields args
      _, _ ->
        Nothing
  ImplDict props ->
    case spine of
      [ ExternAccessor (GetProp prop), ExternApp args ]
        | Just (Tuple analysis' body) <- findProp prop props
        , shouldInlineExternApp qual analysis' body args ->
            Just (evalApp env (eval env body) args)
      _ ->
        Nothing
  _ ->
    Nothing

liftBoolean :: Boolean -> BackendSemantics
liftBoolean = SemNeutral <<< NeutLit <<< LitBoolean

liftInt :: Int -> BackendSemantics
liftInt = SemNeutral <<< NeutLit <<< LitInt

liftNumber :: Number -> BackendSemantics
liftNumber = SemNeutral <<< NeutLit <<< LitNumber

liftString :: String -> BackendSemantics
liftString = SemNeutral <<< NeutLit <<< LitString

foldr1Array :: forall a b. (a -> b -> b) -> (a -> b) -> NonEmptyArray a -> b
foldr1Array f g arr = go (NonEmptyArray.length arr - 2) (g (NonEmptyArray.last arr))
  where
  go ix acc
    | ix < 0 = acc
    | otherwise =
        go (ix - 1) (f (unsafePartial (NonEmptyArray.unsafeIndex arr ix)) acc)

foldl1Array :: forall a b. (b -> a -> b) -> (a -> b) -> NonEmptyArray a -> b
foldl1Array f g arr = go 0 (g (NonEmptyArray.head arr))
  where
  len = NonEmptyArray.length arr
  go ix acc
    | ix == len = acc
    | otherwise =
        go (ix + 1) (f acc (unsafePartial (NonEmptyArray.unsafeIndex arr ix)))

type Ctx =
  { currentLevel :: Int
  , lookupExtern :: Qualified Ident -> Maybe (Tuple BackendAnalysis NeutralExpr)
  , resumeBranches :: Maybe (Tuple (Array (Pair (Lazy BackendSemantics))) (Maybe (Lazy BackendSemantics)))
  }

nextLevel :: Ctx -> Tuple Level Ctx
nextLevel ctx = Tuple (Level ctx.currentLevel) $ ctx { currentLevel = ctx.currentLevel + 1 }

quote :: Ctx -> BackendSemantics -> BackendExpr
quote = go
  where
  go ctx = case _ of
    SemExtern _ _ sem ->
      go ctx (force sem)
    SemLam ident k -> do
      let Tuple level ctx' = nextLevel ctx
      build ctx $ Abs (NonEmptyArray.singleton (Tuple ident level)) $ quote ctx' $ k $ SemNeutral $ NeutLocal ident level
    SemMkFn pro -> do
      let
        loop ctx' idents = case _ of
          MkFnNext ident k -> do
            let Tuple lvl ctx'' = nextLevel ctx'
            loop ctx'' (Array.snoc idents (Tuple ident lvl)) (k (SemNeutral (NeutLocal ident lvl)))
          MkFnApplied body ->
            build ctx' $ UncurriedAbs idents $ quote ctx' body
      loop ctx [] pro
    SemMkEffectFn pro -> do
      let
        loop ctx' idents = case _ of
          MkFnNext ident k -> do
            let Tuple lvl ctx'' = nextLevel ctx'
            loop ctx'' (Array.snoc idents (Tuple ident lvl)) (k (SemNeutral (NeutLocal ident lvl)))
          MkFnApplied body ->
            build ctx' $ UncurriedEffectAbs idents $ quote ctx' body
      loop ctx [] pro
    SemLet ident binding k -> do
      let Tuple level ctx' = nextLevel ctx
      build ctx $ Let ident level (quote ctx binding) $ quote ctx' $ k $ SemNeutral $ NeutLocal ident level
    SemLetRec bindings k -> do
      let Tuple level ctx' = nextLevel ctx
      let neutBindings = (\(Tuple ident _) -> Tuple ident $ defer \_ -> SemNeutral $ NeutLocal (Just ident) level) <$> bindings
      build ctx $ LetRec level
        (map (\b -> quote ctx' $ b neutBindings) <$> bindings)
        (quote ctx' $ k neutBindings)
    SemEffectBind ident binding k -> do
      let Tuple level ctx' = nextLevel ctx
      build ctx $ EffectBind ident level (quote ctx binding) $ quote ctx' $ k $ SemNeutral $ NeutLocal ident level
    SemEffectPure sem ->
      build ctx $ EffectPure (quote ctx sem)
    SemBranch branches def -> do
      let
        ctx' = ctx { resumeBranches = Nothing }
        def' = case def of
          Nothing ->
            quote ctx' <<< uncurry evalBranches <$> ctx.resumeBranches
          Just sem ->
            Just (quote ctx' (force sem))
      build ctx' $ Branch (map (quote ctx' <<< force) <$> branches) def'
    SemBranchTry body branches def -> do
      let
        resumeBranches = case ctx.resumeBranches of
          Just (Tuple prevBranches prevDef) | isNothing def ->
            Just (Tuple (branches <> prevBranches) prevDef)
          _ ->
            Just (Tuple branches def)
      quote (ctx { resumeBranches = resumeBranches }) body
    SemAccessor sem accessor ->
      build ctx $ Accessor (quote ctx sem) accessor
    SemUpdate sem props ->
      build ctx $ Update (quote ctx sem) (map (quote ctx) <$> props)
    SemNeutral neut ->
      quoteNeutral ctx neut

quoteNeutral :: Ctx -> BackendNeutral -> BackendExpr
quoteNeutral ctx = case _ of
  NeutLocal ident level ->
    build ctx $ Local ident level
  NeutVar qual ->
    build ctx $ Var qual
  NeutData qual _ [] ->
    build ctx $ Var qual
  NeutData qual tag values ->
    build ctx $ CtorSaturated qual tag (map (quote ctx) <$> values)
  NeutCtorDef tag fields ->
    build ctx $ CtorDef tag fields
  NeutUncurriedApp hd spine -> do
    let hd' = quote ctx hd
    build ctx $ UncurriedApp hd' (quote ctx <$> spine)
  NeutUncurriedEffectApp hd spine -> do
    let hd' = quote ctx hd
    build ctx $ UncurriedEffectApp hd' (quote ctx <$> spine)
  NeutApp hd spine -> do
    let hd' = quote ctx hd
    case NonEmptyArray.fromArray (quote ctx <$> spine) of
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
  NeutFail err ->
    build ctx $ Fail err

build :: Ctx -> BackendSyntax BackendExpr -> BackendExpr
build ctx = case _ of
  App (ExprSyntax _ (App hd tl1)) tl2 ->
    build ctx $ App hd (tl1 <> tl2)
  Abs ids1 (ExprSyntax _ (Abs ids2 body)) ->
    build ctx $ Abs (ids1 <> ids2) body
  expr@(Let ident1 level1 (ExprSyntax _ (Let ident2 level2 binding2 body2)) body1) ->
    ExprRewrite (withRewrite (analyzeDefault ctx expr)) $ RewriteLetAssoc
      [ { ident: ident2, level: level2, binding: binding2 }
      , { ident: ident1, level: level1, binding: body2 }
      ]
      body1
  expr@(Let ident1 level1 (ExprRewrite _ (RewriteLetAssoc bindings body2)) body1) ->
    ExprRewrite (withRewrite (analyzeDefault ctx expr)) $ RewriteLetAssoc
      (Array.snoc bindings { ident: ident1, level: level1, binding: body2 })
      body1
  Let ident level binding body
    | shouldInlineLet level binding body ->
        rewriteInline ident level binding body
  -- TODO: Multi argument eta reduction?
  -- TODO: Don't eta reduce recursive bindings.
  Abs args (ExprSyntax _ (App hd@(ExprSyntax _ fn) spine))
    | isReference fn
    , [ Tuple _ lvl1 ] <- NonEmptyArray.toArray args
    , [ ExprSyntax _ (Local _ lvl2) ] <- NonEmptyArray.toArray spine
    , lvl1 == lvl2 ->
        hd
  EffectBind ident level (ExprSyntax _ (EffectPure binding)) body ->
    build ctx $ Let ident level binding body
  Branch pairs (Just def) | Just expr <- simplifyBranches ctx pairs def ->
    expr
  PrimOp (Op1 OpBooleanNot (ExprSyntax _ (PrimOp (Op1 OpBooleanNot expr)))) ->
    expr
  expr ->
    buildDefault ctx expr

simplifyBranches :: Ctx -> Array (Pair BackendExpr) -> BackendExpr -> Maybe BackendExpr
simplifyBranches ctx pairs def = case pairs of
  [ a ]
    | Pair expr (ExprSyntax _ (Lit (LitBoolean true))) <- a
    , ExprSyntax _ (Lit (LitBoolean false)) <- def ->
        Just expr
    | Pair expr (ExprSyntax _ (Lit (LitBoolean false))) <- a
    , ExprSyntax _ (Lit (LitBoolean true)) <- def ->
        Just $ build ctx $ PrimOp (Op1 OpBooleanNot expr)
  [ a, b ]
    | Pair expr1@(ExprSyntax _ (Local _ lvl1)) body1 <- a
    , Pair (ExprSyntax _ (PrimOp (Op1 OpBooleanNot (ExprSyntax _ (Local _ lvl2))))) body2 <- b
    , ExprSyntax _ (Fail _) <- def
    , lvl1 == lvl2 ->
        Just $ build ctx $ Branch [ Pair expr1 body1 ] (Just body2)
  [] ->
    Just def
  _
    | ExprSyntax _ (Branch pairs2 def2) <- def ->
        Just $ build ctx (Branch (pairs <> pairs2) def2)
    | otherwise ->
        Nothing

buildDefault :: Ctx -> BackendSyntax BackendExpr -> BackendExpr
buildDefault ctx expr = ExprSyntax (analyzeDefault ctx expr) expr

analyzeDefault :: Ctx -> BackendSyntax BackendExpr -> BackendAnalysis
analyzeDefault ctx = analyze (foldMap fst <<< ctx.lookupExtern)

rewriteInline :: Maybe Ident -> Level -> BackendExpr -> BackendExpr -> BackendExpr
rewriteInline ident level binding body = do
  let
    s2 = analysisOf body
    powAnalysis = case Map.lookup level (unwrap s2).usages of
      Just (Usage { count }) ->
        -- TODO: There may be more work to be done here wrt size.
        s2 <> power (analysisOf binding) count
      Nothing ->
        s2
  ExprRewrite (withRewrite (bound level powAnalysis)) $ RewriteInline ident level binding body

isReference :: forall a. BackendSyntax a -> Boolean
isReference = case _ of
  Var _ -> true
  Local _ _ -> true
  _ -> false

shouldInlineLet :: Level -> BackendExpr -> BackendExpr -> Boolean
shouldInlineLet level a b = do
  let BackendAnalysis s1 = analysisOf a
  let BackendAnalysis s2 = analysisOf b
  case Map.lookup level s2.usages of
    Nothing ->
      true
    Just (Usage { captured, count }) ->
      (s1.complexity == Trivial && s1.size < 5)
        || (not captured && (count == 1 || (s1.complexity <= Deref && s1.size < 5)))
        || (isAbs a && (Map.isEmpty s1.usages || s1.size < 128))

shouldInlineExternApp :: Qualified Ident -> BackendAnalysis -> NeutralExpr -> Spine BackendSemantics -> Boolean
shouldInlineExternApp _ (BackendAnalysis s) _ args =
  (s.complexity == Trivial && s.size < 5)
    || (s.complexity <= Deref && s.size < 5)
    || (Array.length s.args > 0 && Array.length s.args <= Array.length args && s.size < 128)

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
  _ -> false

newtype NeutralExpr = NeutralExpr (BackendSyntax NeutralExpr)

derive instance Newtype NeutralExpr _

optimize :: Ctx -> Env -> BackendExpr -> BackendExpr
optimize ctx env expr1 = do
  let expr2 = quote ctx (eval env expr1)
  case expr2 of
    ExprSyntax (BackendAnalysis { rewrite }) _ | not rewrite ->
      expr2
    _ ->
      optimize ctx env expr2

freeze :: BackendExpr -> Tuple BackendAnalysis NeutralExpr
freeze init = Tuple (analysisOf init) (go init)
  where
  go = case _ of
    ExprSyntax _ expr ->
      NeutralExpr $ go <$> expr
    ExprRewrite _ rewrite ->
      case rewrite of
        RewriteInline ident level binding body ->
          NeutralExpr $ Let ident level (go binding) (go body)
        RewriteLetAssoc bindings body ->
          case NonEmptyArray.fromArray bindings of
            Just bindings' -> do
              let
                { ident, level, binding } = foldl1Array
                  ( \inner outer -> outer
                      { binding =
                          NeutralExpr $ Let inner.ident inner.level inner.binding (go outer.binding)
                      }
                  )
                  (\outer -> outer { binding = go outer.binding })
                  bindings'
              NeutralExpr $ Let ident level binding (go body)
            Nothing ->
              go body

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
