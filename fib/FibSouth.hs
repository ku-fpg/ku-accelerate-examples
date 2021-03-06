{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

import           Prelude hiding (abs)

import           Iter
-- import           If
-- import           TailRec

import           Canonical.Tuple

import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Exp, Lift, lift, Plain, (>*))
import           Data.Array.Accelerate.Interpreter as A

import           Data.Function
import           Data.Bifunctor

fib :: Int -> Int
fib n = iterLoop go (0, 1, n)
    where
      go :: (Int, Int, Int) -> Iter (Int, Int, Int) Int
      go p =
        case tripleCanonical p of
          (a, b, i) ->
            if i > 0
              then step (b, a+b, i-1)
              else done b

fibSpec :: Int -> Int
fibSpec 0 = 1
fibSpec 1 = 1
fibSpec n = fibSpec (n-1) + fibSpec (n-2)

main :: IO ()
main = print (transform (fib 200)) --20))



{-# RULES "Acc-start" [~]
    forall (x :: Int).
    transform x
      =
    transform (rep (abs x))
  #-}

{-# RULES "Acc-finish" [~]
    forall (x :: Exp Int).
    transform (rep x)
      =
    A.indexArray (A.run (A.unit x)) A.Z
  #-}


-- TODO: Make sure it makes sense to have *both* of these elimination
-- rules:
{-# RULES "abs-rep-elim" [~]
    forall x.
    abs (rep x) = x
  #-}
{-# RULES "rep-abs-elim" [~]
    forall x.
    rep (abs x) = x
  #-}

{-# RULES "abs-iterLoop-float" [~]
    forall (f :: (Int, Int, Int) -> Iter (Int, Int, Int) Int)
           (init :: (Int, Int, Int)).
    abs (iterLoop f init)
      =
    iterLoop (iterComp rep abs abs f) (abs init)
  #-}
    -- iterLoop (bimap abs abs . f . rep) (abs init)
    -- iterLoop (iterComp rep abs abs f) (abs init)
    -- iterLoop (abs . f) (rep (abs init))

{-# RULES "cond-intro" [~]
    forall c t (f :: Exp Int).
    rep (if c then t else f)
      =
    rep (A.cond (abs c) t f)
  #-}

{-# RULES "cond-intro-abs" [~]
    forall c t (f :: Bool).
    abs (if c then t else f)
      =
    A.cond (abs c) (abs t) (abs f)
  #-}

{-# RULES "cond-T-intro-cond" [~]
    forall c1 c2 t f1 f2.
    A.cond c1 (if c2 then t else f1) f2
      =
    A.cond c1 (A.cond (abs c2) t f1) f2
  #-}

-- Push abs and rep into the conditionals of fn:
{-# RULES "while-abs-rep-intro-body" [~]
    forall c fn (init :: Exp (Int, Int, Int)).
    A.while c fn init
      =
    A.while c (abs . rep . fn) init
  #-}
{-# RULES "abs-rep-if" [~]
    forall c t (f :: Exp (Int, Int, Int)).
    abs (rep (if c then t else f))
      =
    A.cond (abs c) t f
  #-}
{-# RULES "rep-absImpossible-if" [~]
    forall c t (f :: (Iter (Exp (Int, Int, Int)) (Exp Int))) x y.
    getIter (rep (absImpossible (if c then t else f))) x y
      =
    A.cond (abs c) (getIter t x y :: Exp (Int, Int, Int)) (getIter f x y)
  #-}
{-# RULES "getIter-if" [~]
    forall c t (f :: (Iter (Exp (Int, Int, Int)) (Exp Int))) x y.
    getIter (if c then t else f) x y
      =
    A.cond (abs c) (getIter t x y :: Exp Int) (getIter f x y)
  #-}


{-# RULES "lastStep-rep-absImpossible-intro" [~]
    forall f x.
    lastStep f x
      =
    lastStep (\w -> rep (absImpossible (getIter (f w) step done))) x
  #-}

-- {-# RULES "Iter-abs-rep-intro" [~]
--     forall (body :: forall r. (Exp (Int, Int, Int) -> r) -> (Exp Int -> r) -> r).
--     Iter body
--       =
--     Iter (getIter (bimap (abs . rep) (abs . rep) (Iter (\f g -> body f g))))
--   #-}

-- -- Get rid of reps in 'lastStep' call:
-- {-# RULES "lastCall-abs-rep-intro" [~]
--     forall (f :: Exp (Int, Int, Int) -> Iter (Exp (Int, Int, Int)) (Exp Int))
--            (penultimate :: Exp (Int, Int, Int)).
--     lastStep (Iter f) penultimate
--       =
--     lastStep (id . f) penultimate
--   #-}

-- {-# RULES "test-rule" [~]
--     forall (f :: forall r. (Exp (Int, Int, Int) -> r) -> (Exp Int -> r) -> r) .
--     Iter f
--       =
--     Iter (\g h -> _ $ f (\x -> g x) (\y -> h y))
--   #-}

{-# RULES "abs-float-triple" [~]
    forall (a :: Int) (b :: Int) (c :: Int).
    abs (a, b, c)
      =
    lift (abs a, abs b, abs c)
  #-}
{-# RULES "abs-float-triple1st" [~]
    forall (x :: (Int, Int, Int)).
    abs (triple1st x)
      =
    triple1st (A.unlift (abs x) :: (Exp Int, Exp Int, Exp Int))
  #-}
{-# RULES "abs-float-triple2nd" [~]
    forall (x :: (Int, Int, Int)).
    abs (triple2nd x)
      =
    triple2nd (A.unlift (abs x) :: (Exp Int, Exp Int, Exp Int))
  #-}
{-# RULES "abs-float-triple3rd" [~]
    forall (x :: (Int, Int, Int)).
    abs (triple3rd x)
      =
    triple3rd (A.unlift (abs x) :: (Exp Int, Exp Int, Exp Int))
  #-}

{-# RULES "abs-float-+" [~]
    forall a (b :: Int).
    abs (a + b)
      =
    abs a + abs b
  #-}
{-# RULES "abs-float--" [~]
    forall a (b :: Int).
    abs (a - b)
      =
    abs a - abs b
  #-}


{-# RULES ">*-intro" [~]
    forall a (b :: Int).
    abs (a > b)
      =
    abs a >* abs b
  #-}

{-# RULES "+-intro'" [~]
    forall (f :: (Int, Int, Int) -> Int) g x (y :: (Int, Int, Int)).
    f (rep (abs x)) + g (rep (abs y))
      =
    rep (abs (f x + g y))
  #-}

-- {-# RULES "rep-float-iterLoop" [~]
--     forall (f :: (Int, Int, Int) -> Iter (Exp (Int, Int, Int)) (Exp Int))
--            (init :: (Int, Int, Int)).
--     iterLoop f init
--       =
--     iterLoop (f . rep) (abs init)
--   #-}
--     -- iterLoop (f . rep) (abs init)
--     -- rep (iterLoop (\x -> f (rep x)) (abs init))

{-# RULES "iterToWhile-intro" [~]
    forall (f :: Exp (Int, Int, Int) -> Iter (Exp (Int, Int, Int)) (Exp Int))
           (init :: Exp (Int, Int, Int)).
    iterLoop f init
      =
    iterToWhile f init
  #-}


-- Simple numeric rules: --
{-# RULES ">-intro" [~]
    forall a (b :: Int).
    a > b
      =
    rep (abs a >* abs b)
  #-}

{-# RULES "+-intro" [~]
    forall a (b :: Int).
    a + b
      =
    rep (abs a + abs b)
  #-}

{-# RULES "--intro" [~]
    forall a (b :: Int).
    a - b
      =
    rep (abs a - abs b)
  #-}





-- These should already be done by the time it is in Southbank: --
{-# RULES "done-intro" [~]
    forall f.
    fix f
      =
    iterLoop (done . f (fix f))
  #-}

{-# RULES "step-intro" [~]
    forall f x.
    done (fix f x)
      =
    step x
  #-}

{-# RULES "done-if-float" [~]
    forall c t f.
    done (if c then t else f)
      =
    if c then done t else done f
  #-}


-- {-# RULES "while-intro" [~]
--     forall f.
--   #-}






-- iterLoopA :: ((Int, Int, Int) -> Iter (Int, Int, Int) Int) -> (Int, Int, Int) -> Int
-- iterLoopA f x = (`A.indexArray` A.Z) . run . A.unit $
--   undefined (iterLoop f) $
--     A.while conditional
--             body
--             (A.lift x)
--   where
--     conditional z = getIter (f z) undefined undefined
--     body = undefined

-- {-# RULES "iterLoop->iterLoopA" [~]
--     iterLoop = iterLoopA
--   #-}



