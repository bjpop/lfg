-- print N numbers from the first stream

module Main where

import System.Random.LFG.Pure (sequences)
import System.Random.LFG as LFG (defaultLags, largeLag)
import System.Random.MWC as MWC (create, uniform)
import Control.Monad (replicateM)
import Data.Char (isDigit)
import System (exitFailure)
import IO (hFlush, stdout)
import Data.Word (Word32)

main = do
    n <- getInt "How many numbers to print? "
    mwc <- MWC.create
    let lags = LFG.defaultLags
    initials <- replicateM (largeLag lags) (MWC.uniform mwc)
    let seq = head $ sequences lags initials
    mapM_ print $ take n seq

getInt :: String -> IO Int
getInt message = do
    putStr message
    hFlush stdout
    line <- getLine
    if all isDigit line
       then return $ read line
       else do
          putStrLn $ "Error: " ++ line ++ " is not a positive integer"
          exitFailure
