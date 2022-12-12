module Snapshot.DefaultRulesFunction01 where

import Prelude

type F = forall a b c. a -> b -> c

test1 :: F -> F -> Unit -> Unit
test1 f g a = f 1 $ g "foo" $ a

test2 :: F -> F -> Unit -> Unit
test2 f g a = a # g "foo" # f 1

test3 :: F -> F -> Unit -> Unit
test3 f g _ = flip f 3 $ flip g 2 1

test4 :: F -> F
test4 f = flip f

test5 :: forall a b. a -> b -> a
test5 a = const a

test6 :: forall a. a -> a
test6 = flip const 42
