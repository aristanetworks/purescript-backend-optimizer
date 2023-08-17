let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230721/packages.dhall
        sha256:8800ac7d0763826544ca3ed3ba61f9dcef761a9e2a1feee0346437d9b861e78f

in  upstream
  with lib-backend-optimizer = ../lib/spago.dhall as Location
  with processors-backend-optimizer = ../processors/spago.dhall as Location
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
