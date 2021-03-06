{-# LANGUAGE DeriveFunctor #-}

module If where

data If a b
  = If
    { getCond  :: (a -> Bool)
    , getTrue  :: b
    , getFalse :: b
    }
    deriving Functor

runIf :: (a -> If a b) -> a -> b
runIf f x =
  let i = f x
  in
  if getCond i x
    then getTrue  i
    else getFalse i

