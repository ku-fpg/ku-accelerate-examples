module TailRec where

import           Iter
import           If

type TailRec a b = a -> If a (Iter a b)

runTailRec :: TailRec a b -> a -> b
runTailRec = iterLoop . runIf

