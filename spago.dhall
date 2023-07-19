{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "backend-optimizer"
, dependencies =
  [ "aff"
  , "ansi"
  , "argonaut"
  , "argonaut-codecs"
  , "argparse-basic"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "dodo-printer"
  , "effect"
  , "either"
  , "enums"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "language-cst-parser"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-glob-basic"
  , "node-path"
  , "node-process"
  , "node-streams"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "posix-types"
  , "prelude"
  , "refs"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "backend-es/src/**/*.purs", "backend-es/test/*.purs" ]
}
