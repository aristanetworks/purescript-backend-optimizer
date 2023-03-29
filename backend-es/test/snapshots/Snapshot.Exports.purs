module Snapshot.Exports
  ( Data_Unused_TypeOnly
  , Data_Used_TypeOnly
  , useData_Used_TypeOnly
  , Data_TypeAndCtor(..)
  , Newtype_Unused_TypeOnly
  , Newtype_Used_TypeOnly
  , useNewtype_Used_TypeOnly
  , Newtype_TypeAndCtor(..)
  , TypeExported
  , class Class_Exported
  , classExported
  ) where

data Data_Unused_TypeOnly = Data_Unused_TypeOnly

data Data_Used_TypeOnly = Data_Used_TypeOnly Int

useData_Used_TypeOnly :: Data_Used_TypeOnly -> Int
useData_Used_TypeOnly (Data_Used_TypeOnly i) = i

data Data_TypeAndCtor = Data_TypeAndCtor Int

newtype Newtype_Unused_TypeOnly = Newtype_Unused_TypeOnly Int

newtype Newtype_Used_TypeOnly = Newtype_Used_TypeOnly Int

useNewtype_Used_TypeOnly :: Newtype_Used_TypeOnly -> Int
useNewtype_Used_TypeOnly (Newtype_Used_TypeOnly i) = i

newtype Newtype_TypeAndCtor = Newtype_TypeAndCtor Int

type TypeNoExport = Int

type TypeExported = Int

class Class_NoExport_Unused a where
  classNoExportUnused :: a -> String

class Class_NoExport_Used a where
  classNoExportUsed :: a -> String

data Data_Instance_Unexported = Data_Instance_Unexported

instance Class_NoExport_Used Data_Instance_Unexported where
  classNoExportUsed _ = "Data_Instance_Unexported"

class Class_Exported a where
  classExported :: a -> String
