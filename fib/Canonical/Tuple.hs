module Canonical.Tuple where

pairCanonical :: (a, b) -> (a, b)
pairCanonical p = (fst p, snd p)

