module Snapshot.Exports
  ( DataTypeOnly
  , useDataTypeOnly
  , DataTypeOnlyCtorNameChange
  , useDataTypeOnlyCtorNameChange
  , DataTypeAndCtor(..)
  , NewtypeTypeOnly
  , useNewtypeTypeOnly
  , NewtypeTypeAndCtor(..)
  , TypeExported
  , class ClassExported
  , classExportedMember
  ) where

data DataTypeOnly = DataTypeOnly Int

useDataTypeOnly :: DataTypeOnly -> Int
useDataTypeOnly (DataTypeOnly i) = i

data DataTypeOnlyCtorNameChange = DataTypeOnlyCtor Int

useDataTypeOnlyCtorNameChange :: DataTypeOnlyCtorNameChange -> Int
useDataTypeOnlyCtorNameChange (DataTypeOnlyCtor i) = i

data DataTypeAndCtor = DataTypeAndCtor Int

newtype NewtypeTypeOnly = NewtypeTypeOnly Int

useNewtypeTypeOnly :: NewtypeTypeOnly -> Int
useNewtypeTypeOnly (NewtypeTypeOnly i) = i

newtype NewtypeTypeAndCtor = NewtypeTypeAndCtor Int

type TypeNoExport = Int

type TypeExported = Int

class ClassExported a where
  classExportedMember :: a -> String

class ClassNoExport a where
  classNoExportMember :: a -> String

