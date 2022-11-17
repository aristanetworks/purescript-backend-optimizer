module Snapshot.DefaultRulesContravariant01 where

import Prelude

import Data.Const (Const(..))
import Data.Functor.Contravariant (imapC, (>#<))
import Data.Op (Op(..))

test1 = (Const 1 :: Const Int Int) >#< (_ + 1)

test2 = imapC (_ + 9) (_ + 1) (Const 1 :: Const Int Int)

test3 = (Op \i -> i + 3) >#< (_ + 1)

test4 = imapC (_ + 9) (_ + 1) (Op \i -> i + 4)
