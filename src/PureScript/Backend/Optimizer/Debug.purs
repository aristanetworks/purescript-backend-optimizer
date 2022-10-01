module PureScript.Backend.Optimizer.Debug where

import Prelude

import Debug (class DebugWarning)

traceWhen :: forall a b. DebugWarning => Boolean -> a -> b -> b
traceWhen bool a b = if bool then traceImpl a \_ -> b else b

spyWhen :: forall a. DebugWarning => Boolean -> a -> a
spyWhen bool a = traceWhen bool a a

foreign import time_ :: forall a. String -> (Unit -> a) -> a

foreign import traceImpl :: forall a b. a -> (Unit -> b) -> b

time :: forall a. DebugWarning => String -> (Unit -> a) -> a
time = time_
