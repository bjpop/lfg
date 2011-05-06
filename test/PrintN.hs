-- print N numbers from the first stream

module Main where

import Random.LFG (streams)
import Data.Char (isDigit)
import System (exitFailure)
import IO (hFlush, stdout)

main = do
    n <- getInt "How many numbers to print? "
    mapM_ print $ take n $ head streams

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
