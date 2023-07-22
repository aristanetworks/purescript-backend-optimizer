{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "snapshots-backend-optimizer"
, dependencies =
  [ "arrays"
  , "console"
  , "convertable-options"
  , "effect"
  , "either"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "heterogeneous"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "refs"
  , "safe-coerce"
  , "st"
  , "strings"
  , "tuples"
  , "type-equality"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
