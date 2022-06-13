{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "functions"
  , "minibench"
  , "prelude"
  ]
, packages = ../packages.dhall
, sources = [ "./*.purs" ]
}
