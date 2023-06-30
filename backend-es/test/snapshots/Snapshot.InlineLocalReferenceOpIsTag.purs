module Snapshot.InlineLocalReferenceOpIsTag where

data Unit = Unit

shouldInlineIsTag :: Unit -> Unit
shouldInlineIsTag a = case a of
  Unit -> a

test :: Unit
test = shouldInlineIsTag Unit
