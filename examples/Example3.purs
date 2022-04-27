module Example3 where

data Maybe a = Nothing | Just a

foreign import plus :: forall a. a -> a -> a

append Nothing a = a
append a Nothing = a
append (Just a) (Just b) = Just a

