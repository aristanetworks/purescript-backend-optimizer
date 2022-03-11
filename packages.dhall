let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220224/packages.dhall sha256:67cc3d4f0e8fb72bb1413ba94ddd72a3ceb0783eb725e3b22ad7568b3b581163

in  upstream
  with dodo-printer =
    { dependencies =
      [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
    , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
    , version = "v2.2.0"
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
  with node-workerbees =
    { dependencies =
      [ "aff"
      , "argonaut-core"
      , "arraybuffer-types"
      , "avar"
      , "effect"
      , "either"
      , "exceptions"
      , "maybe"
      , "newtype"
      , "parallel"
      , "variant"
      ]
    , repo = "https://github.com/natefaubion/purescript-node-workerbees.git"
    , version = "v0.2.1"
    }
  with argparse-basic =
    { dependencies =
      [ "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "record"
      , "strings"
      ]
    , repo = "https://github.com/natefaubion/purescript-argparse-basic.git"
    , version = "v1.0.0"
    }
