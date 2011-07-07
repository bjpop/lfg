{-# LANGUAGE DeriveDataTypeable #-}
module Random.LFG (lfg, Gen (..), generators, step, GenST, stepST, initST, Element, chunks) where

import Prelude hiding (init)
import Data.Array.ST (STUArray, newListArray)
import Control.Monad.ST.Strict as S (ST)
import Control.Monad.ST.Lazy as L (ST, runST, strictToLazyST)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throw)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Random.LFG.Init as Init (randomSequence, Element)
import System.Random.MWC (create)

newtype Gen = Gen { genElements :: [Element] }
step :: Gen -> (Element, Gen)
step (Gen (x:xs)) = (x, Gen xs)
step _ = error $ "generator was empty"

generators :: [Gen]
generators = map (Gen . lfg) $ chunks initK Init.randomSequence

type Index = Int -- unsafe read/write require Int arguments
type LagTable s = STUArray s Index Element

initJ, initK :: Index
initJ = 861
initK = 1279

-- STUArray is faster than STArray.
-- strictness annotations in Gen do make appreciable difference to time/space,
-- especially with unpack pragmas.

data GenST s =
   GenST
   { lagTable :: LagTable s -- the last k values in the sequence
   , j :: {-# UNPACK #-} !Index -- the position of the smaller lag
   , k :: {-# UNPACK #-} !Index -- the position of the larger lag
   , pos :: {-# UNPACK #-} !Index -- the position of the next element in the sequence
   , lagSize :: Index -- size of the lag table, must be equal to k
   }

data LFGException = InitLagException
   deriving (Show, Typeable)

instance Exception LFGException

-- initialise a RNG using the small lag, large lag and list of
-- seed elements (these should be "random"), ie use another random
-- number generator to make them.
-- init :: Index -> Index -> [Element] -> S.ST s (Gen s)
initST :: Index -> Index -> [Element] -> S.ST s (GenST s)
initST j k is = do
   when (j >= k) $ throw InitLagException
   array <- newListArray (0, k-1) is
   return $ GenST { lagTable = array
                , j = j-1
                , k = k-1
                , lagSize = k
                , pos = 0
                }

-- an explicit INLINE pragma on step makes a difference when step is exported.
-- unsafe read and write drop a non-trivial amount of time, between 1-10%

{-# INLINE stepST #-}
stepST :: GenST s -> S.ST s (Element, GenST s)
stepST gen@(GenST { lagTable = array, j = oldJ, k = oldK, lagSize = size, pos = currentPos }) = do
   jth <- unsafeRead array oldJ
   kth <- unsafeRead array oldK
   -- x_i = (x_j + x_k) mod 1
   -- is there a haskell equivalent to fmod?
   let (_, nextElement) = properFraction (jth + kth) :: (Integer, Double)
   unsafeWrite array currentPos nextElement
   let newJ = nextIndex oldJ
       newK = nextIndex oldK
       newPos = nextIndex currentPos
   return (nextElement, gen { pos = newPos, j = newJ, k = newK })
   where
   nextIndex :: Index -> Index
   nextIndex i = if i == 0 then size - 1 else i - 1

genRands :: GenST s -> L.ST s [Element]
genRands gen = do
   (next, newGen) <- strictToLazyST $ stepST gen
   ys <- genRands newGen
   -- XXX should we force the value of next in the result?
   -- may get thunk-leak otherwise
   return (next : ys)

lfgST :: [Element] -> L.ST s [Element]
lfgST is = do
   gen <- strictToLazyST $ initST initJ initK is
   genRands gen

lfg :: [Element] -> [Element]
lfg seed = L.runST $ lfgST seed

chunks :: Int -> [a] -> [[a]]
chunks size xs
   = case splitAt size xs of
        (c,rest) -> c : chunks size rest
