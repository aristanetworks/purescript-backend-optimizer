-- @inline Data.Lens.Internal.Forget.choiceForget(..).left arity=1
-- @inline Data.Lens.Internal.Forget.choiceForget(..).right arity=1
module Snapshot.ProfunctorLenses02 where

import Prelude

import Data.Lens (_Left, _Right, preview)

test1 = preview _Left
test2 a = preview _Left a
test3 = preview (_Left <<< _Right)
test4 a = preview (_Left <<< _Right) a
