{ name = "snapshots"
, dependencies =
  [ "arrays"
  , "console"
  , "convertable-options"
  , "effect"
  , "either"
  , "functions"
  , "heterogeneous"
  , "integers"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "random"
  , "record"
  , "refs"
  , "tuples"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "./*.purs", "./*/*.purs" ]
}
