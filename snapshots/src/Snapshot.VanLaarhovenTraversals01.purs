-- @inline export traverseFun1 arity=1
-- @inline export rewriteBottomUpM arity=1
module Snapshot.VanLaarhovenTraversals01 where

import Prelude

import Data.Identity (Identity(..))
import Safe.Coerce (coerce)

data Fun
  = Abs String Fun
  | App Fun Fun

traverseFun1 :: forall f. Applicative f => (Fun -> f Fun) -> Fun -> f Fun
traverseFun1 k = case _ of
  Abs id a ->
    Abs id <$> k a
  App a b ->
    App <$> k a <*> k b

rewriteBottomUpM :: forall m. Monad m => (Fun -> m Fun) -> Fun -> m Fun
rewriteBottomUpM k = k <=< go
  where
  go a = traverseFun1 (k <=< go) a

rewriteBottomUp :: (Fun -> Fun) -> Fun -> Fun
rewriteBottomUp = coerce (rewriteBottomUpM :: _ -> _ -> Identity _)
