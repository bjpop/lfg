-- print the nthRandom number from nGens number of streams

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
    nthRandom <- getInt "Which position in the random sequence? "
    nGens <- getInt "How many generators? "
    mwc <- MWC.create
    let lags = LFG.defaultLags
    initials <- replicateM (nGens * largeLag lags) (MWC.uniform mwc)
    let seqs = take nGens $ sequences lags initials
    mapM_ (print . (!! (nthRandom - 1))) seqs

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
