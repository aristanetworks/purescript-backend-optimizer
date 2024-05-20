let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221018/packages.dhall
        sha256:b1db2e4a17260ace8d17858602f8c56f460982d6e404818d7f6cb9f053324bb1
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
      , "node-fs-aff"
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
  with json =
    { dependencies =
      [ "prelude"
      , "functions"
      , "integers"
      , "maybe"
      , "either"
      , "tuples"
      , "foldable-traversable"
      , "gen"
      , "strings"
      , "unfoldable"
      ]
    , repo = "https://github.com/purescript/purescript-json.git"
    , version = "da4695707d8aacd54e7cbbd54c069509248ff989"
    }
