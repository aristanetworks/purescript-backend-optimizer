{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lib-backend-optimizer"
, dependencies =
  ["argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "control"
  , "debug"
  , "dodo-printer"
  , "either"
  , "enums"
  , "free"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "language-cst-parser"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs" ]
}
