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
  , "strings"
  , "tuples"
  , "type-equality"
  , "variant"
  ]
, packages = ../../../packages.dhall
, sources = [ "./*.purs", "./*/*.purs" ]
}
