module Canonical.Function where

functionCanonical :: (a -> b) -> (a -> b)
functionCanonical f = \x -> (f x)

