module Snapshot.Html (Html, test) where

import Prelude hiding (discard)

import Data.Array as Array

class BuildArray a b c | a b -> c where
  buildArray :: a -> b -> Array c

instance BuildArray (Array a) (Array a) a where
  buildArray = append
else instance BuildArray a (Array a) a where
  buildArray = Array.cons
else instance BuildArray (Array a) a a where
  buildArray = Array.snoc
else instance BuildArray a a a where
  buildArray a b = [ a, b ]

discard :: forall a b c. BuildArray a b c => a -> (Unit -> b) -> Array c
discard a k = buildArray a (k unit)

data Html = Elem String (Array Html) | Text String

class BuildChildren a where
  buildChildren :: a -> Array Html

instance BuildChildren Html where
  buildChildren = pure

instance BuildChildren (Array Html) where
  buildChildren = identity

section :: forall a. BuildChildren a => a -> Html
section = Elem "section" <<< buildChildren

article :: forall a. BuildChildren a => a -> Html
article = Elem "article" <<< buildChildren

h1 :: forall a. BuildChildren a => a -> Html
h1 = Elem "h1" <<< buildChildren

h2 :: forall a. BuildChildren a => a -> Html
h2 = Elem "h2" <<< buildChildren

p :: forall a. BuildChildren a => a -> Html
p = Elem "p" <<< buildChildren

text :: String -> Html
text = Text

test :: String -> Html
test user = section do
  h1 do
    text $ "Posts for " <> user
  article do
    h2 do
      text "The first post"
    p do
      text "This is the first post."
      text "Not much else to say."
