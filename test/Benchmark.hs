{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad.ST
import Criterion.Main
import Data.Int
import Data.Word
import qualified System.Random as R
import System.Random.MWC
import qualified System.Random.Mersenne as M
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import System.IO.Unsafe ( unsafePerformIO )
import Random.LFG as LFG (Gen, step, generators)
import Sprng

main = do
  mwc <- create
  mtg <- M.newMTGen . Just =<< uniform mwc
  genRef <- newIORef $ head LFG.generators
  gen1 :: RNG LFG <- newRng
  initRng gen1 0 1 985456376 0
  defaultMain
    [ bgroup "mwc"
      [ bench "Double"  (uniform mwc :: IO Double)
      ]
    , bgroup "random"
      [
        bench "Double" (R.randomIO >>= evaluate :: IO Double)
      ]
    , bgroup "mersenne"
      [
        bench "Double" (M.random mtg :: IO Double)
      ]
    , bgroup "LFG"
      [
        bench "Double" (lfgIO genRef)
      ]
    , bgroup "SPRNG"
      [
        bench "Double" (randomDouble gen1)
      ]
    ]

{-
{-# NOINLINE lfgGen #-}
lfgGen :: IORef Gen
lfgGen = unsafePerformIO $ newIORef $ head generators
-}

lfgIO :: IORef LFG.Gen -> IO Double
lfgIO genRef = do
    thisGen <- readIORef genRef
    let (nextVal, nextGen) = LFG.step thisGen
    writeIORef genRef nextGen
    return nextVal
