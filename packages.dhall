let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20241231/packages.dhall
        sha256:61533129f1e3369392665ecd389caecf1da74bec897e7ff355e5d920c92dda61

in  upstream
  with dodo-printer =
    { dependencies =
      [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
    , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
    , version = "v2.2.1"
    }
  with node-glob-basic =
    { dependencies =
      [ "aff"
      , "console"
      , "effect"
      , "lists"
      , "maybe"
      , "node-fs"
      , "node-path"
      , "node-process"
      , "ordered-collections"
      , "strings"
      ]
    , repo = "https://github.com/natefaubion/purescript-node-glob-basic.git"
    , version = "v1.2.2"
    }
  with arrays.version = "v7.2.1"
  with ordered-collections.version = "v3.1.0"
