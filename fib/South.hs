-- | Combines the elements of the southbank language into one data type

{-# LANGUAGE GADTs, KindSignatures, DeriveFunctor, StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds, PolyKinds, TypeOperators #-}
module South where

import           Iter
import           TailRec
import           If

-- import           Data.Proxy
-- import           Data.Type.Equality

data South a b where
  IfS   :: Bool -> (a -> South a b) -> (a -> South a b) -> South a b
  DoneS :: b -> South a b
  StepS :: a -> South a b

ifS :: Bool -> (a -> South a b) -> (a -> South a b) -> South a b
ifS = IfS

doneS :: b -> South a b
doneS = DoneS

stepS :: a -> South a b
stepS = StepS

runS :: (a -> South a b) -> a -> b
runS f x =
  case f x of
    IfS False t f -> runS f x

