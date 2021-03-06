{-
   Generate an initial random sequence to seed the LFG.
-}
module Random.LFG.Init (randomSequence, Element) where

import System.Random.MWC as MWC (GenST, uniform, create)
import Control.Monad.ST.Lazy as L (ST, runST, strictToLazyST)

-- XXX this doesn't belong here, temporary measure
type Element = Double

genRands :: GenST s -> L.ST s [Element]
genRands gen = do
   next <- strictToLazyST $ uniform gen
   ys <- genRands gen
   -- Must force the eval of next, otherwise we could end up with
   -- a thunk-leak in the output list
   return (seq next (next : ys))

randomSequence :: [Element]
randomSequence = L.runST (genRands =<< strictToLazyST MWC.create)
