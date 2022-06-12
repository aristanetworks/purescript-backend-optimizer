module PureScript.Backend.Debug where

import Debug (class DebugWarning, trace)

traceWhen :: forall a b. DebugWarning => Boolean -> a -> b -> b
traceWhen bool a b = if bool then trace a \_ -> b else b

spyWhen :: forall a. DebugWarning => Boolean -> a -> a
spyWhen bool a = traceWhen bool a a