{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "processors-backend-optimizer"
, dependencies = [ "lib-backend-optimizer"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
