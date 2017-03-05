module Canonical.Tuple where

pairCanonical :: (a, b) -> (a, b)
pairCanonical p = (fst p, snd p)

tripleCanonical :: (a, b, c) -> (a, b, c)
tripleCanonical p = (triple1st p, triple2nd p, triple3rd p)

triple1st :: (a, b, c) -> a
triple2nd :: (a, b, c) -> b
triple3rd :: (a, b, c) -> c
triple1st (x, _, _) = x
triple2nd (_, y, _) = y
triple3rd (_, _, z) = z
{-# NOINLINE triple1st #-}
{-# NOINLINE triple2nd #-}
{-# NOINLINE triple3rd #-}
