-- @inline Snapshot.RecursionInlined07.foldlArray always
-- @inline Snapshot.RecursionInlined07.foldlArray2 always
-- @inline Snapshot.RecursionInlined07.nutsToHtml always

-- Another "real-world" example from deku
module Snapshot.RecursionInlined07 where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Global as Region
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)

foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldlArray bab b arr = foldlArray2 0 len bab b arr
  where
  len = Array.length arr

foldlArray2 :: forall a b. Int -> Int -> (b -> a -> b) -> b -> Array a -> b
foldlArray2 n i bab b arr
  | n == i = b
  | otherwise = foldlArray2 (n + 1) i bab (bab b (unsafePartial $ Array.unsafeIndex arr n)) arr

data FEvent

newtype Cb = Cb (FEvent -> Effect Boolean)

data AttributeValue = Prop' String | Cb' Cb | Unset'

newtype VolatileAttribute = VolatileAttribute
  { key :: String
  , value :: AttributeValue
  }
newtype ActualizedKorok = ActualizedKorok
  ( Either String
      { html :: String
      , attributes :: Object (Event VolatileAttribute)
      , nuts :: Object Korok
      , count :: Int
      }
  )

newtype Event a = Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype Element interpreter r payload = Element
  (PSR r -> interpreter -> Event payload)

data Child (logic :: Type) (obj :: Type)
  = Insert (Entity logic obj)
  | Remove
  | Logic logic

newtype DynamicChildren logic obj = DynamicChildren
  (Event (Event (Child logic obj)))

newtype FixedChildren logic obj = FixedChildren
  (Array (Entity logic obj))

newtype EventfulElement logic obj = EventfulElement
  (Event (Entity logic obj))

newtype DOMInterpret :: forall k. k -> Type
newtype DOMInterpret payload = DOMInterpret { foobar :: Unit -> DOMInterpret payload }

data Scope = Local String | Global
type PSR r =
  { parent :: Maybe String
  , scope :: Scope
  , raiseId :: String -> ST.ST Region.Global Unit
  | r
  }
newtype Node payload = Node
  ( PSR (pos :: Maybe Int, ez :: Boolean, dynFamily :: Maybe String)
    -> DOMInterpret payload
    -> Event payload
  )

data Entity logic obj
  = DynamicChildren' (DynamicChildren logic obj)
  | FixedChildren' (FixedChildren logic obj)
  | EventfulElement' (EventfulElement logic obj)
  | Element' obj
type Nut' payload = Entity Int (Node payload)

-- | _The_ type that represents a Deku application. To be used when `Nut` doesn't cut it
-- | (for example, when different locks need to be used because of the use of portals).
newtype NutF payload = NutF (Nut' payload)

newtype PureKorok = PureKorok
  ({ count :: Int } -> ActualizedKorok)

newtype Korok = Korok (forall payload. NutF payload)
newtype Nut = Nut (Either PureKorok Korok)

delimiter :: String
delimiter = "qrs"

nutsToHtml :: Array (Either ActualizedKorok Korok) -> Int -> String
nutsToHtml actualized count = acc
  where
  (Tuple acc _) = foldlArray
    ( \(Tuple acc c) -> case _ of
        Left (ActualizedKorok k) -> case k of
          Left txt -> Tuple (acc <> txt) c
          Right { html } -> Tuple (acc <> html) c
        Right _ ->
          Tuple (acc <> delimiter <> show c <> delimiter) (c + 1)
    )
    (Tuple "" count)
    actualized

test1 :: String
test1 = nutsToHtml
  [ Left $ ActualizedKorok $ Right { count: 0, html: "<div></div>", attributes: Object.empty, nuts: Object.empty }
  , Left $ ActualizedKorok $ Right { count: 0, html: "<h1></h1>", attributes: Object.empty, nuts: Object.empty }
  , Left $ ActualizedKorok $ Right { count: 0, html: "<b></b>", attributes: Object.empty, nuts: Object.empty }
  , Left $ ActualizedKorok $ Right { count: 0, html: "<i></i>", attributes: Object.empty, nuts: Object.empty }
  ]
  0