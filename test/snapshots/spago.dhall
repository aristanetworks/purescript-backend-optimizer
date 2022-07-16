{ name = "snapshots"
, dependencies =
  [ "arrays"
  , "console"
  , "convertable-options"
  , "effect"
  , "functions"
  , "heterogeneous"
  , "integers"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "random"
  , "record"
  , "tuples"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "./*.purs", "./*/*.purs" ]
}
