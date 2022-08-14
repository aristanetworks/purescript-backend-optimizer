{ name = "snapshots"
, dependencies =
  [ "arrays"
  , "console"
  , "convertable-options"
  , "effect"
  , "either"
  , "exists"
  , "foldable-traversable"
  , "functions"
  , "heterogeneous"
  , "integers"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "refs"
  , "strings"
  , "tuples"
  , "type-equality"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "./*.purs", "./*/*.purs" ]
}
