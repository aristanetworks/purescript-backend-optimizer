let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220822/packages.dhall
        sha256:908b4ffbfba37a0a4edf806513a555d0dbcdd0cde7abd621f8d018d2e8ecf828

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
