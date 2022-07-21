module Snapshot.DefaultRulesFunctor01 where

import Prelude
import Data.Maybe (Maybe)

foreign import mb :: forall a. Maybe a

test1 = mb <#> \(i :: Int) -> show i
test2 = void mb
test3 = mb $> 42
test4 = 42 <$ mb
test5 = const <$> mb <@> 12
