module Snapshot.DefaultRulesFunction01 where

import Prelude
import Data.Function (on)

foreign import f :: forall a b c. a -> b -> c
foreign import g :: forall a b c. a -> b -> c

test1 a = f 1 $ g "foo" $ a
test2 a = a # g "foo" # f 1
test3 a = flip f 3 $ flip g 2 1
test4 = flip f
test5 a = const a
test6 = flip const 42
