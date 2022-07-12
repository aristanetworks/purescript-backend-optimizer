{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "functions"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "random"
  , "record"
  , "heterogeneous"
  , "variant"
  , "tuples"
  ]
, packages = ../packages.dhall
, sources = [ "./*.purs" ]
}
