{-# LANGUAGE Strict #-}

import           Iter
import           Canonical.Tuple

fib :: Int -> Int
fib n = start (go (0, 1, n))
  where

