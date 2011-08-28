-- Based on the uniform.hs test from mwc-random package.
-- Copyright 2009, 2010, 2011 Bryan O'Sullivan.
--
-- Modified by Bernie Pope to suit the LFG.
--
-- Tests for testing uniformity of distributions
--
-- Require statistics >= 0.7

import qualified Data.IntMap as IntMap
import Statistics.Distribution
import Statistics.Distribution.ChiSquared
import Random.LFG as LFG
import Text.Printf

-- Generate a vector of frequency counts for a certain number of bins.
-- If there are N bins and E expected samples, then we generate N * E
-- random numbers, put them in bins, and count the frequency of the
-- numbers that fall in each bin.
fill :: Int    -- expected number of items per bin.
     -> Int    -- number of bins
     -> Gen    -- random generator
     -> [Int]  -- list of counts of values in each bin
fill n numBins gen =
   IntMap.elems $ loop 0 IntMap.empty gen
   where
   loop :: Int -> IntMap.IntMap Int -> Gen -> IntMap.IntMap Int
   loop k bins gen
      | k == n * numBins = bins
      | otherwise = let (nextVal, nextGen) = step gen
                        thisBinPos = truncate (nextVal * fromIntegral numBins)
                        nextBins = IntMap.insertWith (\_ old -> old + 1) thisBinPos 1 bins
                    in loop (k+1) nextBins nextGen

-- Calculate χ² statistics for vector of number occurences for
-- hypotheshys that each bin has equal probability
chi2uniform :: [Int] -> Double
chi2uniform v = (sum $ map (sqr . subtract u . fromIntegral) v) / u
  where
    n   = length v
    tot = sum v
    u   = fromIntegral tot / fromIntegral n
    sqr x = x * x

checkChi2 :: Double             -- Desired significance level
          -> [Int]              -- Vector of values
          -> IO ()
checkChi2 p v = do
  let x2   = chi2uniform v      -- Observed χ²
      ndf  = length v - 1       -- N degrees of freedom
      d    = chiSquared ndf     -- Theoretical distribution
      pLow = cumulative d x2
      pHi  = 1 - pLow

  putStrLn $ if pLow > p && (1-pLow) > p then "OK" else "* FAILED *"
  printf "  significance = %.3f\n"   p
  printf "  x2/ndf = %.3f\n"        (x2 / fromIntegral ndf)
  printf "  p(x2 < observed) = %.3g\n" pLow
  printf "  p(x2 > observed) = %.3g\n" pHi

main :: IO ()
main = do
  putStrLn "100000 vals and 1000 bins"
  let v1 = fill 10000 100 (head LFG.generators)
  checkChi2 0.05 v1
  checkChi2 0.01 v1
  putStrLn ""
