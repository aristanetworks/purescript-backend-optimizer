module PureScript.Backend.Optimizer.CoreFn where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.CodeUnits as SCU
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import PureScript.Backend.Optimizer.Hash (thenHash)

newtype Ident = Ident String

derive newtype instance eqIdent :: Eq Ident
derive newtype instance ordIdent :: Ord Ident
derive newtype instance hashableIdent :: Hashable Ident
derive instance Newtype Ident _

newtype ModuleName = ModuleName String

derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive newtype instance hashableModuleName :: Hashable ModuleName
derive instance Newtype ModuleName _

newtype ProperName = ProperName String

derive newtype instance eqProperName :: Eq ProperName
derive newtype instance ordProperName :: Ord ProperName
derive newtype instance hashableProperName :: Hashable ProperName

data Qualified a = Qualified (Maybe ModuleName) a

derive instance eqQualified :: Eq a => Eq (Qualified a)
derive instance ordQualified :: Ord a => Ord (Qualified a)
instance hashableQualified :: Hashable a => Hashable (Qualified a) where
  hash (Qualified mn a) = hash mn `thenHash` a

derive instance Functor Qualified

unQualified :: forall a. Qualified a -> a
unQualified (Qualified _ a) = a

qualifiedModuleName :: forall a. Qualified a -> Maybe ModuleName
qualifiedModuleName (Qualified mn _) = mn

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
  | IsSyntheticApp

derive instance eqMeta :: Eq Meta
derive instance ordMeta :: Ord Meta

data ConstructorType
  = ProductType
  | SumType

derive instance eqConstructorType :: Eq ConstructorType
derive instance ordConstructorType :: Ord ConstructorType
instance hashableConstructorType :: Hashable ConstructorType where
  hash ProductType = 1119464941
  hash SumType = 1602057837


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

moduleName :: forall a. Module a -> ModuleName
moduleName (Module mod) = mod.name

data Import a = Import a ModuleName

derive instance functorImport :: Functor Import

importName :: forall a. Import a -> ModuleName
importName (Import _ name) = name

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
  | ExprConstructor a ProperName Ident (Array String)
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

derive instance Eq a => Eq (Prop a)
derive instance functorProp :: Functor Prop

instance hashableProp :: Hashable a => Hashable (Prop a) where
  hash (Prop s a) = hash s `thenHash` a

instance foldableProp :: Foldable Prop where
  foldl k a (Prop _ b) = k a b
  foldr k b (Prop _ a) = k a b
  foldMap k (Prop _ a) = k a

instance traversableProp :: Traversable Prop where
  traverse k (Prop str a) = Prop str <$> k a
  sequence (Prop str a) = Prop str <$> a

propKey :: forall a. Prop a -> String
propKey (Prop k _) = k

propValue :: forall a. Prop a -> a
propValue (Prop _ a) = a

findProp :: forall a. String -> Array (Prop a) -> Maybe a
findProp prop = Array.findMap (\(Prop k v) -> if prop == k then Just v else Nothing)

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

isPrimModule :: ModuleName -> Boolean
isPrimModule (ModuleName name) = name == "Prim" || SCU.take 5 name == "Prim."
