module PureScript.CoreFn where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse)

newtype Ident = Ident String

derive newtype instance eqIdent :: Eq Ident
derive newtype instance ordIdent :: Ord Ident

newtype ModuleName = ModuleName String

derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName

newtype ProperName = ProperName String

derive newtype instance eqProperName :: Eq ProperName
derive newtype instance ordProperName :: Ord ProperName

data Qualified a = Qualified (Maybe ModuleName) a

derive instance eqQualified :: Eq a => Eq (Qualified a)
derive instance ordQualified :: Ord a => Ord (Qualified a)

unQualified :: forall a. Qualified a -> a
unQualified (Qualified _ a) = a

type SourcePos =
  { line :: Int
  , column :: Int
  }

type SourceSpan =
  { path :: String
  , start :: SourcePos
  , end :: SourcePos
  }

newtype Ann = Ann
  { span :: SourceSpan
  , meta :: Maybe Meta
  }

data Meta
  = IsConstructor ConstructorType (Array Ident)
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere

derive instance eqMeta :: Eq Meta
derive instance ordMeta :: Ord Meta

data ConstructorType
  = ProductType
  | SumType

derive instance eqConstructorType :: Eq ConstructorType
derive instance ordConstructorType :: Ord ConstructorType

data Comment
  = LineComment String
  | BlockComment String

newtype Module a = Module
  { name :: ModuleName
  , path :: String
  , span :: SourceSpan
  , imports :: Array (Import a)
  , exports :: Array Ident
  , reExports :: Array ReExport
  , decls :: Array (Bind a)
  , foreign :: Array Ident
  , comments :: Array Comment
  }

data Import a = Import a ModuleName

derive instance functorImport :: Functor Import

data ReExport = ReExport ModuleName Ident

derive instance eqReExport :: Eq ReExport
derive instance ordReExport :: Ord ReExport

data Bind a
  = NonRec (Binding a)
  | Rec (Array (Binding a))

derive instance functorBind :: Functor Bind

data Binding a = Binding a Ident (Expr a)

derive instance functorBinding :: Functor Binding

data Expr a
  = ExprVar a (Qualified Ident)
  | ExprLit a (Literal (Expr a))
  | ExprConstructor a ProperName Ident (Array Ident)
  | ExprAccessor a (Expr a) String
  | ExprUpdate a (Expr a) (Array (Prop (Expr a)))
  | ExprAbs a Ident (Expr a)
  | ExprApp a (Expr a) (Expr a)
  | ExprCase a (Array (Expr a)) (Array (CaseAlternative a))
  | ExprLet a (Array (Bind a)) (Expr a)

derive instance functorExpr :: Functor Expr

data CaseAlternative a = CaseAlternative (Array (Binder a)) (CaseGuard a)

derive instance functorCaseAlternative :: Functor CaseAlternative

data CaseGuard a
  = Unconditional (Expr a)
  | Guarded (Array (Guard a))

derive instance functorCaseGuard :: Functor CaseGuard

data Guard a = Guard (Expr a) (Expr a)

derive instance functorGuard :: Functor Guard

data Prop a = Prop String a

derive instance functorProp :: Functor Prop

instance foldableProp :: Foldable Prop where
  foldl k a (Prop _ b) = k a b
  foldr k b (Prop _ a) = k a b
  foldMap k (Prop _ a) = k a

instance traversableProp :: Traversable Prop where
  traverse k (Prop str a) = Prop str <$> k a
  sequence (Prop str a) = Prop str <$> a

propValue :: forall a. Prop a -> a
propValue (Prop _ a) = a

data Literal a
  = LitInt Int
  | LitNumber Number
  | LitString String
  | LitChar Char
  | LitBoolean Boolean
  | LitArray (Array a)
  | LitRecord (Array (Prop a))

derive instance functorLiteral :: Functor Literal

instance foldableLiteral :: Foldable Literal where
  foldl k = foldlDefault k
  foldr k = foldrDefault k
  foldMap k = case _ of
    LitArray as -> foldMap k as
    LitRecord ps -> foldMap (foldMap k) ps
    _ -> mempty

instance traversableLiteral :: Traversable Literal where
  traverse k = case _ of
    LitArray as -> LitArray <$> traverse k as
    LitRecord ps -> LitRecord <$> traverse (traverse k) ps
    LitInt a -> pure (LitInt a)
    LitNumber a -> pure (LitNumber a)
    LitString a -> pure (LitString a)
    LitChar a -> pure (LitChar a)
    LitBoolean a -> pure (LitBoolean a)
  sequence a = sequenceDefault a

data Binder a
  = BinderNull a
  | BinderVar a Ident
  | BinderNamed a Ident (Binder a)
  | BinderLit a (Literal (Binder a))
  | BinderConstructor a (Qualified ProperName) (Qualified Ident) (Array (Binder a))

derive instance functorBinder :: Functor Binder

emptyAnn :: Ann
emptyAnn = Ann
  { span: emptySpan
  , meta: Nothing
  }

emptySpan :: SourceSpan
emptySpan = { path: "<internal>", start: zero, end: zero }

exprAnn :: forall a. Expr a -> a
exprAnn = case _ of
  ExprVar a _ -> a
  ExprLit a _ -> a
  ExprConstructor a _ _ _ -> a
  ExprAccessor a _ _ -> a
  ExprUpdate a _ _ -> a
  ExprAbs a _ _ -> a
  ExprApp a _ _ -> a
  ExprCase a _ _ -> a
  ExprLet a _ _ -> a

litChildren :: forall a. Literal a -> Array a
litChildren = case _ of
  LitArray as -> as
  LitRecord ps -> propValue <$> ps
  _ -> []