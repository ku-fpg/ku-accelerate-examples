{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}

import           Iter

fib :: Int -> Int
fib n = iterLoop go (0, 1, n)
    where
      go :: (Int, Int, Int) -> Iter (Int, Int, Int) Int
      go (a, b, 0) = done b
      go (a, b, i) = step (b, (a+b), (i-1))

main :: IO ()
main = print (fib 20)

