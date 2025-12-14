module Snapshot.PrimOpBooleanNotRegression where

import Prelude

-- Related to #93.
-- Inlines to multiple references to the result of comp. The call
-- to comp should not be duplicated due to OpBooleanNot.
test :: forall a. (a -> a -> Ordering) -> a -> a -> Boolean
test comp a b = comp a b /= EQ
