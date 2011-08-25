-- print N numbers from the first stream

module Main where

import Random.LFG (generators, Gen, step)
import Data.Char (isDigit)
import System (exitFailure)
import IO (hFlush, stdout)

main = do
    n <- getInt "How many numbers to print? "
    mapM_ print $ take n $ mkStream $ head generators

mkStream :: Gen -> [Double]
mkStream gen = nextVal : mkStream nextGen
   where
   (nextVal, nextGen) = step gen

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
