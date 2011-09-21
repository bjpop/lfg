module Main where

import System.Random.LFG (Gen, initRng, uniform)
import Control.Monad (replicateM_)
import Data.Int (Int32)

main = do
   gen <- initRng 0 1 42 0
   replicateM_ 100 (print =<< (uniform gen :: IO Int32))
   replicateM_ 100 (print =<< (uniform gen :: IO Float))
   replicateM_ 100 (print =<< (uniform gen :: IO Double))
   return ()
