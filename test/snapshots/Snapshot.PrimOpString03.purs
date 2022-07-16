module Snapshot.PrimOpString03 where

import Prelude

foreign import a :: String

test1 = "a" <> "b" <> a <> "c" <> "d"
test2 = "a" <> (("b" <> a) <> "c") <> "d"
test3 = "a" <> ("b" <> (a <> "c")) <> "d"
