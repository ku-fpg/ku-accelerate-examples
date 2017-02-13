{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}

import           Data.Array.Accelerate
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate.Interpreter

tripleSnd :: (a, b, c) -> b
tripleSnd (_, y, _) = y

-- | Specialized version to avoid ambiguous types
unliftTriple :: Exp (Int, Int, Int) -> (Exp Int, Exp Int, Exp Int)
unliftTriple = unlift

fib :: Exp Int -> Exp Int
fib = tripleSnd . unliftTriple . go 0 1
  where
    go :: Exp Int -> Exp Int -> Exp Int -> Exp (Int, Int, Int)
    go a b i =
      while (\(unliftTriple -> (_, _, i)) -> i >* 0)
            (\(unliftTriple -> (a, b, i)) -> lift (b, a+b, i-1))
            (lift (a, b, i) :: Exp (Int, Int, Int))

    -- go a b 0 = b
    -- go a b i = go b (a+b) (i-1)

fibSpec :: Int -> Int
fibSpec 0 = 1
fibSpec 1 = 1
fibSpec n = fibSpec (n - 1) + fibSpec (n - 2)

main :: IO ()
main = print (run (unit (fib 20)))

