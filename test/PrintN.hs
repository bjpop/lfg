-- print N numbers from the first stream

module Main where

import System.Random.LFG as LFG (init, uniform)
import Control.Monad (replicateM_, when)
import Data.Char (isDigit)
import System (exitFailure)
import IO (hFlush, stdout)

main = do
    n <- getInt "How many numbers to print? "
    gen <- LFG.init 42 0
    replicateM_ n (print =<< (uniform gen :: IO Double))

getInt :: String -> IO Int
getInt message = do
    putStr message
    hFlush stdout
    line <- getLine
    if (length line > 0)
       then if all isDigit line
               then return $ read line
               else do
                  putStrLn $ "Error: " ++ line ++ " is not a positive integer"
                  exitFailure
       else do
          putStrLn $ "Error: empty input"
          exitFailure
