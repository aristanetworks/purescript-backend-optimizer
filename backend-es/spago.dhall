{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "backend-optimizer"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argparse-basic"
  , "arrays"
  , "bifunctors"
  , "lib-backend-optimizer"
  , "processors-backend-optimizer"
  , "console"
  , "control"
  , "dodo-printer"
  , "effect"
  , "either"
  , "enums"
  , "filterable"
  , "foldable-traversable"
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
  , "strings"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs" ]
}
