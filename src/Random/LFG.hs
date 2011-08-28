{-# LANGUAGE DeriveDataTypeable #-}
-- | This library implements a Lagged (Additive) Fibonacci Generator,
-- yielding psuedo random sequences of Double precision numbers in the
-- range [0,1).
--
-- An individual sequence is produced by a generator, represented by the
-- "Gen" type. A single generator can be used to produce an infinite, but
-- periodic sequence of numbers. The period should be sufficiently large
-- for most practical purposes.
--
-- An infinite, but periodic, list of generators is provided by the
-- "generators" constant. If you need 100 generators then use:
--
-- > gens = take 100 generators
--
-- Each generator should be different (up until the full cycle of generators
-- is exhausted).
--
-- The next value in a sequence can be obtained by applying the "step"
-- function to a generator:
--
-- > (nextVal, nextGen) = step gen
--
-- The result is the next value in the sequence and a new generator.
--
-- Here is a program which prints the 1000000th number from 100 different generators:
--
-- >import Random.LFG (Gen, generators, step)
-- >main = do
-- >   let gens = take 100 generators
-- >   mapM_ (print . genNth 1000000) gens
-- >
-- >genNth :: Int -> Gen -> Double
-- >genNth 1 gen = fst $ step gen
-- >genNth n gen = genNth (n-1) $ snd $ step gen

module Random.LFG
   ( -- * Purely functional interface
     Gen (..)
   , generators
   , step
   , lfg
   , Element
     -- * Imperative interface
   , GenST
   , stepST
   , initST
     -- * utilities
   , chunks
   ) where

import Prelude hiding (init)
import Data.Array.ST (STUArray, newListArray)
import Control.Monad.ST.Strict as S (ST)
import Control.Monad.ST.Lazy as L (ST, runST, strictToLazyST)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throw)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Random.LFG.Init as Init (randomSequence, Element)

-- | A generator of an infinite (but periodic) sequence of pseudo random numbers.
newtype Gen = Gen { genElements :: [Element] }

-- | A step function on generators. Given a generator @step@ will yield the next
-- number in the sequence and a new generator. The new generator can be used to
-- produce the next-next number in the sequence and so forth.
step :: Gen -> (Element, Gen)
step (Gen (x:xs)) = (x, Gen xs)
-- this should never happen.
step _ = error $ "generator was empty"

-- | An infinite (but periodic) sequence of generators. Up until the limits
-- of the period, each generator should produce a different pseudo random
-- sequence of numbers.
generators :: [Gen]
generators = map (Gen . lfg) $ chunks initK Init.randomSequence

-- | indices into the lag table, unsafe read/write require Int arguments
type Index = Int
-- | the lag table, keeping the last K values in the sequence
-- STUArray is faster than STArray.
type LagTable s = STUArray s Index Element

-- | hard coded values for the lags J and K, these must be carefully chosen
initJ, initK :: Index
initJ = 861
initK = 1279

-- strictness annotations in Gen do make appreciable difference to time/space,
-- especially with unpack pragmas.
data GenST s =
   GenST
   { lagTable :: LagTable s       -- ^ the last K values in the sequence
   , j :: {-# UNPACK #-} !Index   -- ^ the position of the smaller lag
   , k :: {-# UNPACK #-} !Index   -- ^ the position of the larger lag
   , pos :: {-# UNPACK #-} !Index -- ^ the position of the next element in the sequence
   , lagSize :: Index             -- ^ size of the lag table, must be equal to K
   }

-- | An exception type to indicate that the generator was not initialised correctly,
-- namely that the value of J was >= K.
data LFGException = InitLagException
   deriving (Show, Typeable)

instance Exception LFGException

-- | Initialise a LFG using the small lag, large lag and a finite list of
-- seed elements (these should be "random"), ie use another random
-- number generator to make them. The input list of random numbers must
-- have length >= K, the size of the lag table.
initST :: Index -> Index -> [Element] -> S.ST s (GenST s)
initST j k is = do
   -- check that the lags are appropriate
   when (j >= k) $ throw InitLagException
   -- check that enough initial values have been provided
   when (length is < k) $ throw InitLagException
   -- initialise a new lag table from the input elements
   array <- newListArray (0, k-1) is
   -- return the initial state of the generator
   return $ GenST { lagTable = array
                , j = j-1  -- indices are zero-based
                , k = k-1  -- indices are zero-based
                , lagSize = k
                , pos = 0  -- current position is at the start of the table
                }

-- | Advance the generator by one step, yielding the next value in the sequence
-- and a new generator state.
{-# INLINE stepST #-}
-- an explicit INLINE pragma on step makes a difference when step is exported.
stepST :: GenST s -> S.ST s (Element, GenST s)
stepST gen@(GenST { lagTable = array, j = oldJ, k = oldK, lagSize = size, pos = currentPos }) = do
   -- the "unsafe" reads are actually safe because we know they will be in bounds of the lag table.
   -- unsafe read and write drop a non-trivial amount of time, between 1-10%
   jth <- unsafeRead array oldJ
   kth <- unsafeRead array oldK
   -- compute the next element from: x_i = (x_j + x_k) mod 1
   -- is there a haskell equivalent to fmod?
   let (_, nextElement) = properFraction (jth + kth) :: (Integer, Double)
   -- overwrite the last value in the table with the new element
   unsafeWrite array currentPos nextElement
   -- advance the lag indices and wrap around if necessary
   let newJ = nextIndex oldJ
       newK = nextIndex oldK
       newPos = nextIndex currentPos
   return (nextElement, gen { pos = newPos, j = newJ, k = newK })
   where
   -- increment an index and wrap to zero if we reach the end of the table
   nextIndex :: Index -> Index
   nextIndex i = if i == 0 then size - 1 else i - 1

-- | Run a generator to produce a list of elements lazily.
genRands :: GenST s -> L.ST s [Element]
genRands gen = do
   (next, newGen) <- strictToLazyST $ stepST gen
   ys <- genRands newGen
   -- XXX should we force the value of next in the result?
   -- may get thunk-leak otherwise
   return (next : ys)

-- | Given a finite list of initial random values, rturn a ST computation yielding a
-- infinite sequence of random numbers. NOTE: the input sequence of
-- random numbers must have length at least K (the size of the lag table).
lfgST :: [Element] -> L.ST s [Element]
lfgST is = do
   -- initialise the generator
   gen <- strictToLazyST $ initST initJ initK is
   -- start producing the sequence
   genRands gen

-- | A pure interface to the LFG generator, given an initial sequence of random numbers.
lfg :: [Element] -> [Element]
lfg seed = L.runST $ lfgST seed

chunks :: Int -> [a] -> [[a]]
chunks size xs
   = case splitAt size xs of
        (c,rest) -> c : chunks size rest
