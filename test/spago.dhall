{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "tests-backend-optimizer"
, dependencies =
  [ "aff"
  , "ansi"
  , "backend-optimizer"
  , "argparse-basic"
  , "arrays"
  , "lib-backend-optimizer"
  , "processors-backend-optimizer"
  , "console"
  , "dodo-printer"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "language-cst-parser"
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
  , "posix-types"
  , "prelude"
  , "refs"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs" ]
}
