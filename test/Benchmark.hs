{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad.ST
import Criterion.Main
import Data.Int
import Data.Word
import qualified System.Random as R
import System.Random.MWC as MWC
import qualified System.Random.Mersenne as M
import System.Random.LFG as LFG (GenIO, uniform, init)
import Control.Monad (replicateM)
import Sprng


main = do
  mwc <- MWC.create
  mtg <- M.newMTGen . Just =<< MWC.uniform mwc
  lfgGen <- LFG.init 42 0
  sprngGen <- Sprng.new 42
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
    , bgroup "SPRNG"
      [
        bench "Double" (Sprng.randomDouble sprngGen :: IO Double)
      ]
    ]
