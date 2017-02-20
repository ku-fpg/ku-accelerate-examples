-- | Combines the elements of the southbank language into one data type

{-# LANGUAGE GADTs, KindSignatures, DeriveFunctor, StandaloneDeriving #-}

module South where

import           Iter
import           TailRec
import           If

data South :: * -> * -> * where
  IfS   :: If   a b -> Maybe (South x b) -> South a b
  IterS :: Iter a b -> Maybe (South x b) -> South a b

deriving instance Functor (South a)

stepS :: b -> South b a
stepS = (`IterS` Nothing) . step

doneS :: a -> South b a
doneS = (`IterS` Nothing) . done

-- XXX: Does this make sense with the type as it is now?
runSouth :: (a -> South a b) -> a -> b
runSouth f x =
  case f x of
    IfS   i k -> runIf    (const i) x
    IterS i k -> iterLoop (const i) x

