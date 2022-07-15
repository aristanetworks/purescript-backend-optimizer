{ name = "snapshots"
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
, packages = ./packages.dhall
, sources = [ "./*.purs", "./*/*.purs" ]
}
