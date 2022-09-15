module Snapshot.GenericBounded01 where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Generic.Rep (class Generic)

data Test1 = Bottom1 | Ignored1 | Top1

derive instance Eq Test1
derive instance Ord Test1
derive instance genericTest1 :: Generic Test1 _

instance Bounded Test1 where
  bottom = genericBottom
  top = genericTop

-- data Test4 = Bottom4 | Ignored4 | Top4

-- derive instance Eq Test4
-- derive instance Ord Test4
-- derive instance genericTest4 :: Generic Test4 _

-- instance Bounded Test4 where
--   bottom = genericBottom
--   top = genericTop

-- data Test5 = Bottom5 | Ignored5 | Top5

-- derive instance Eq Test5
-- derive instance Ord Test5
-- derive instance genericTest5 :: Generic Test5 _

-- instance Bounded Test5 where
--   bottom = genericBottom
--   top = genericTop

-- data Test6 = Bottom6 | Ignored6 | Top6

-- derive instance Eq Test6
-- derive instance Ord Test6
-- derive instance genericTest6 :: Generic Test6 _

-- instance Bounded Test6 where
--   bottom = genericBottom
--   top = genericTop
