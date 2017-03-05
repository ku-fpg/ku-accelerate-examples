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
  DoneS :: a -> South 

