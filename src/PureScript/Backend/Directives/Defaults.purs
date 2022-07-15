module PureScript.Backend.Directives.Defaults where

defaultDirectives :: String
defaultDirectives =
  """
  Control.Apply.applyFirst arity=2
  Control.Apply.applySecond arity=2
  Control.Category.categoryFn.identity always
  Control.Monad.ST.Internal.modify arity=2
  Control.Semigroupoid.composeFlipped arity=2
  Control.Semigroupoid.semigroupoidFn.compose arity=2
  Data.Function.const arity=1
  Effect.Ref.modify arity=2
  Record.Builder.build arity=1
  Record.Builder.rename arity=8
  """
