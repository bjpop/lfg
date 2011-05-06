-- print the nthRandom number from nGens number of streams

module Main where

import Random.LFG (streams)
import Data.Char (isDigit)
import System (exitFailure)
import IO (hFlush, stdout)

main = do
    nthRandom <- getInt "Which position in the random sequence? "
    nGens <- getInt "How many generators? "
    mapM_ print $ take nGens $ map (head . drop nthRandom) streams

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
