{-# LANGUAGE Strict #-}

import           Iter
import           If
import           TailRec

fib :: Int -> Int
fib n = runTailRec go (0, 1, n)
    where
      -- go :: (Int, Int, Int) -> If (Int, Int, Int) (Iter (Int, Int, Int) Int)
      go :: TailRec (Int, Int, Int) Int
      go (a, b, i)
        = If (\(a, b, i) -> i > 0)
             (step (b, (a+b), (i-1)))
             (done b)

fibSpec :: Int -> Int
fibSpec 0 = 1
fibSpec 1 = 1
fibSpec n = fibSpec (n-1) + fibSpec (n-2)

main :: IO ()
main = print (fib 20)

