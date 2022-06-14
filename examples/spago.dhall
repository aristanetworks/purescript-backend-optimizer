{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "functions"
  , "minibench"
  , "prelude"
  ]
, packages = ../packages.dhall
, sources = [ "./*.purs" ]
}
