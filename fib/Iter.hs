{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Iter where

import           Prelude hiding (abs)

import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Exp, Lift, lift, Plain, (>*))
import           Data.Array.Accelerate.Interpreter as A

import           Data.Bifunctor

abs :: (Lift Exp a, a ~ Plain a) => a -> Exp a
abs x = lift x
{-# NOINLINE abs #-}

-- | All calls to 'absImpossible' should be gone by the time compilation finishes.
absImpossible :: a -> Exp a
absImpossible _ = error "Internal error: absImpossible called"

-- | All calls to 'rep' should be gone by the time compilation finishes.
rep :: Exp a -> a
rep _ = error "Internal error: rep called"
{-# NOINLINE rep #-}

-- newtype Iter a b = Iter { getIter :: forall r. (a -> r) -> (b -> r) -> r }
-- data Iter a b = Iter (forall r. (a -> r) -> (b -> r) -> r)
--   deriving (Functor)

data Iter a b
  = Step a
  | Done b
  deriving Functor

instance Bifunctor Iter where
  first f (Step a) = Step (f a)
  second = fmap

iterBimap :: (a -> a') -> (b -> b') -> Iter a b -> Iter a' b'
iterBimap f g (Step a) = Step (f a)
iterBimap f g (Done b) = Done (g b)

getIter :: Iter a b -> (forall r. (a -> r) -> (b -> r) -> r)
getIter (Step a) f _ = f a
getIter (Done b) _ g = g b
{-# NOINLINE getIter #-}

step :: a -> Iter a b
step = Step
{-# NOINLINE step #-}

done :: b -> Iter a b
done = Done
{-# NOINLINE done #-}

iterLoop :: (a -> Iter a b) -> a -> b
iterLoop f x = getIter (f x) (iterLoop f) id
{-# NOINLINE iterLoop #-}

-- -- iterLoop' :: (Lift Exp a) => (a -> Iter (Exp a) b) -> a -> b
-- iterLoop' :: (Exp (Int, Int, Int) -> Iter (Exp (Int, Int, Int)) (Exp Int))
--                -> Exp (Int, Int, Int) -> Exp Int
-- iterLoop' f x = getIter (f (abs (rep x))) (iterLoop (f . abs . rep)) id
-- {-# NOINLINE iterLoop' #-}

-- | Turn the last step into an `id`
doneToId :: (a -> Iter a b) -> (a -> Iter a a)
doneToId f x = fmap (const x) (f x)

loopBody :: (a -> Iter a a) -> (a -> a)
loopBody f x = getIter (f x) id id

-- | This should only be used when a single step is performed (and the
-- recursion inside of 'iterLoop' will not be performed.
-- TODO: Add machinery to verify this restriction.
lastStep :: (a -> Iter a b) -> a -> b
lastStep f x = -- getIter (f x) f' id
  case f x of
    Done r -> r
    Step a ->
      case f a of
        Done r -> r
        _      -> error "lastStep: Should be actual last step"
    -- _ -> error "lastStep: Shouldn't run after actual last step"
  -- where
  --   f' z =
  --     case f z of
  --       Done r -> r
  --       Step _ -> error "lastStep: Should be actual last step"
{-# NOINLINE lastStep #-}

iterComp :: (a' -> a) -> (a -> a') -> (b -> b') -> (a -> Iter a b) -> (a' -> Iter a' b')
iterComp a'a aa' bb' iter x =
  -- Iter $ \f g -> getIter (iter (a'a x)) (f . aa') (g . bb')
  iterBimap aa' bb' (iter (a'a x))

iterToWhile :: (A.Elt a, A.Elt b) => (Exp a -> Iter (Exp a) (Exp b)) -> Exp a -> Exp b
iterToWhile f init
  = lastStep f
    $ A.while (abs . conditional)
              body
              init
  where
    (loop, (body, conditional)) = splitLoop f
{-# INLINE iterToWhile #-}

splitLoop :: (a -> Iter a b) -> (a -> b, (a -> a, a -> Bool))
splitLoop f = (loop, (loopBody (doneToId f), getCondition . f))
  where
    loop = iterLoop f

getCondition :: Iter b a -> Bool
-- getCondition f = getIter f (const False) (const True)
getCondition f = getIter f (const True) (const False)


-- | Mark the part of the code to be transformed
transform :: a -> a
transform x = x
{-# NOINLINE transform #-}

