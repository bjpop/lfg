-- print N numbers from the first stream

module Main where

import Random.MersenneTwister (mersenneTwister)
import Data.Char (isDigit)
import System (exitFailure)
import IO (hFlush, stdout)

main = do
    seed <- getInt "Seed? "
    n <- getInt "How many numbers? "
    mapM_ print $ take n $ mersenneTwister (fromIntegral seed)

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
