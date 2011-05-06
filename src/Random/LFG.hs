{-# LANGUAGE DeriveDataTypeable #-}
module Random.LFG (lfg, streams, Gen, step, init, Element) where

import Prelude hiding (init)
import Data.Array.ST (STUArray, newListArray)
import Control.Monad.ST.Strict as S (ST)
import Control.Monad.ST.Lazy as L (ST, runST, strictToLazyST)
import Data.Word (Word32)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throw)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Random.MersenneTwister (mersenneTwister)

type Element = Word32
type Index = Int -- unsafe read/write require Int arguments
type LagTable s = STUArray s Index Element

initJ, initK :: Index
initJ = 861
initK = 1279

-- STUArray is faster than STArray.
-- strictness annotations in Gen do make appreciable difference to time/space,
-- especially with unpack pragmas.

data Gen s =
   Gen
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
init :: Index -> Index -> [Element] -> S.ST s (Gen s)
init j k is = do
   when (j >= k) $ throw InitLagException
   array <- newListArray (0, k-1) is
   return $ Gen { lagTable = array
                , j = j-1
                , k = k-1
                , lagSize = k
                , pos = 0
                }

-- an explicit INLINE pragma on step makes a difference when step is exported.
-- unsafe read and write drop a non-trivial amount of time, between 1-10%

{-# INLINE step #-}
step :: Gen s -> S.ST s (Element, Gen s)
step gen@(Gen { lagTable = array, j = oldJ, k = oldK, lagSize = size, pos = currentPos }) = do
   jth <- unsafeRead array oldJ
   kth <- unsafeRead array oldK
   -- add one to avoid potential avalanche of zeros b/c M*0=0 and 0*M=0
   let nextElement = (jth * kth) + 1
   unsafeWrite array currentPos nextElement
   let newJ = nextIndex oldJ
       newK = nextIndex oldK
       newPos = nextIndex currentPos
   return (nextElement, gen { pos = newPos, j = newJ, k = newK })
   where
   nextIndex :: Index -> Index
   nextIndex i = if i == 0 then size - 1 else i - 1

genRands :: Gen s -> L.ST s [Element]
genRands gen = do
   (next, newGen) <- strictToLazyST $ step gen
   ys <- genRands newGen
   return (next : ys)

lfgST :: [Element] -> L.ST s [Element]
lfgST is = do
   gen <- strictToLazyST $ init initJ initK is
   genRands gen

lfg :: [Element] -> [Element]
lfg seed = L.runST $ lfgST seed

streams :: [[Element]]
streams = map lfg $ chunks initK $ mersenneTwister 1

chunks :: Int -> [a] -> [[a]]
chunks size xs
   = c : chunks size rest
   where
   (c, rest) = splitAt size xs
