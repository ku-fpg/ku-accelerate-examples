{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import           Prelude hiding (abs)

import           Iter
-- import           If
-- import           TailRec

import           Canonical.Tuple

import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Exp, Lift, lift, Plain, (>*))
import           Data.Array.Accelerate.Interpreter as A

import           Data.Function

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
main = print (transform (fib 20))


abs :: (Lift Exp a, a ~ Plain a) => a -> Exp a
abs = lift
{-# NOINLINE abs #-}

-- | All calls to 'rep' should be gone by the time compilation finishes.
rep :: Exp a -> a
rep _ = error "Internal error: rep called"
{-# NOINLINE rep #-}


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


{-# RULES "abs-rep-elim" [~]
    forall x.
    abs (rep x) = x
  #-}

{-# RULES "abs-iterLoop-float" [~]
    forall f (init :: (Int, Int, Int)).
    abs (iterLoop f init)
      =
    iterLoop (iterComp rep abs abs f) (abs init)
  #-}
    -- iterLoop (abs . f) (rep (abs init))

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



