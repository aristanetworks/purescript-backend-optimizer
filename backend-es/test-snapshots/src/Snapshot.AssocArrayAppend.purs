module Snapshot.AssocArrayAppend where

import Prelude

test1 :: Array String -> Array String
test1 x = [ "a" ] <> ((((([ "b" ] <> x) <> x) <> x) <> x) <> [ "c" ]) <> [ "d" ]

test2 :: Array String -> Array String
test2 x = [ "a" ] <> ([ "b" ] <> (x <> (x <> (x <> (x <> [ "c" ]))))) <> [ "d" ]

test3 :: Array String -> Array String
test3 x = [ "a" ] <> ([ "b" ] <> (x <> (x <> (x <> (x <> [ "c" ]))))) <> [ "d" ] <> ((((([ "e" ] <> x) <> x) <> x) <> x) <> [ "f" ]) <> [ "g" ]
