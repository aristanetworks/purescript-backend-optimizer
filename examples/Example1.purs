module Example1 where

foreign import data Effect :: Type -> Type

foreign import bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b

foreign import pure :: forall a. a -> Effect a

foreign import log :: String -> Effect Unit

foreign import data Unit :: Type

foreign import unit :: Unit

data Maybe a = Nothing | Just a

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe def = case _ of
  Nothing -> def
  Just a -> a

map :: forall a b. (a -> b) -> Maybe a -> Maybe b
map f = case _ of
  Nothing -> Nothing
  Just a -> Just (f a)

main :: Effect Unit
main = do
  _ <- log "Hello, world!"
  _ <- log (fromMaybe "uh oh" (map (\_ -> "ok") (Just "foo")))
  pure unit