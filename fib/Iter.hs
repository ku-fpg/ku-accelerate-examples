{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Iter where

newtype Iter a b = Iter { getIter :: forall r. (a -> r) -> (b -> r) -> r }
  deriving (Functor)

step :: b -> Iter b a
step x = Iter $ \f g -> f x

done :: a -> Iter b a
done x = Iter $ \f g -> g x

iterLoop :: (a -> Iter a b) -> a -> b
iterLoop f x = getIter (f x) (iterLoop f) id

