{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad.ST
import Criterion.Main
import Data.Int
import Data.Word
import qualified System.Random as R
import System.Random.MWC as MWC
import qualified System.Random.Mersenne as M
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import System.IO.Unsafe ( unsafePerformIO )
import System.Random.LFG as LFG (GenIO, uniform, create, defaultLags, largeLag)
import Control.Monad (replicateM)

main = do
  mwc <- MWC.create
  mtg <- M.newMTGen . Just =<< MWC.uniform mwc
  let lags = LFG.defaultLags
  initials <- replicateM (largeLag lags) (MWC.uniform mwc)
  [lfgGen] <- LFG.create lags 1 (initials :: [Word32])
  defaultMain
    [ bgroup "mwc"
      [ bench "Double"  (MWC.uniform mwc :: IO Double)
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
        bench "Double" (LFG.uniform lfgGen :: IO Double)
      ]
    ]