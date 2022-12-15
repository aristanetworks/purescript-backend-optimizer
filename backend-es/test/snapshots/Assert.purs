module Assert where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)

-- For some reason Test.Assert always logs the error, which we don't want.
assertEqual :: forall a. Eq a => Show a => String -> { expected :: a, actual :: a } -> Effect Unit
assertEqual label { expected, actual }
  | expected == actual = mempty
  | otherwise =
      throw $ label
        <> "\n"
        <> "Expected: "
        <> show expected
        <> "\n"
        <> "Actual:   "
        <> show actual
