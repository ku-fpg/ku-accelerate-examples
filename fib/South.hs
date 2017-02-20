-- | Combines the elements of the southbank language into one data type

{-# LANGUAGE GADTs, KindSignatures, DeriveFunctor, StandaloneDeriving #-}

module South where

import           Iter
import           TailRec
import           If

import           Control.Monad.Free

data SouthF :: * -> * where
  IfS   :: If   a b -> SouthF (a -> b) -> SouthF b
  IterS :: Iter a b -> SouthF (a -> b) -> SouthF b
  Done  :: SouthF b

deriving instance Functor SouthF

type South = Free SouthF

stepS :: b -> South a
stepS = liftF . (`IterS` Done) . step

doneS :: a -> South a
doneS = liftF . (`IterS` Done) . done

-- XXX: Does this make sense with the type as it is now?
iterLoopS :: (a -> South b) -> a -> b
iterLoopS f x = undefined

