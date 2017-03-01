-- | Combines the elements of the southbank language into one data type

{-# LANGUAGE GADTs, KindSignatures, DeriveFunctor, StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds, PolyKinds, TypeOperators #-}
module South where

import           Iter
import           TailRec
import           If

import           Data.Proxy
import           Data.Type.Equality

data South :: * -> * -> * where
  IfS   :: (a -> If   a b) -> (b -> South b c) -> South a c
  IterS :: (a -> Iter a b) -> (b -> South b c) -> South a c
  FinishS :: b -> South a b

deriving instance Functor (South a)

stepS :: b -> South b a
stepS x = IterS (const (step x)) FinishS

doneS :: a -> South b a
doneS x = IterS (const (done x)) FinishS

-- XXX: Does this make sense with the type as it is now?
runSouth :: (a -> South a b) -> a -> b
runSouth f x =
  case f x of
    IfS   i k -> runSouth k (runIf    i x)
    IterS i k -> runSouth k (iterLoop i x)

