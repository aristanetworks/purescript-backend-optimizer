-- @inline ConvertableOptions.convertRecordOptionsCons arity=6
-- @inline ConvertableOptions.convertRecordOptionsNil always
-- @inline export flub always
-- @inline export flubImpl never
module Snapshot.ConvertableOptions01
  ( test1
  , test2
  , test3
  , test4
  ) where

import Prelude
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe(..))

type Optional =
  ( foo :: Int
  , baz :: Maybe Boolean
  )

type All =
  ( bar :: String
  | Optional
  )

data Flub = Flub

instance convertFlubBar1 :: ConvertOption Flub "bar" Int String where
  convertOption _ _ int = show int

else instance convertFlubBar2 :: ConvertOption Flub "bar" String String where
  convertOption _ _ str = str

else instance convertFlubBaz1 :: ConvertOption Flub "baz" Boolean (Maybe Boolean) where
  convertOption _ _ bool = Just bool

else instance convertFlubBaz2 :: ConvertOption Flub "baz" (Maybe Boolean) (Maybe Boolean) where
  convertOption _ _ mb = mb

else instance convertFlubDefault :: ConvertOption Flub sym a a where
  convertOption _ _ a = a

defaultOptions :: { | Optional }
defaultOptions =
  { foo: 42
  , baz: Nothing
  }

flub
  :: forall provided
   . ConvertOptionsWithDefaults Flub { | Optional } { | provided } { | All }
  => { | provided }
  -> String
flub provided = flubImpl all
  where
  all :: { | All }
  all = convertOptionsWithDefaults Flub defaultOptions provided

flubImpl :: { | All } -> String
flubImpl _ = "???"

test1 :: String
test1 = flub { bar: "Hello" }

test2 :: String
test2 = flub { foo: 99, bar: "Hello" }

test3 :: String
test3 = flub { foo: 99, bar: "Hello", baz: Just true }

test4 :: String
test4 = flub { foo: 99, bar: 42, baz: true }
