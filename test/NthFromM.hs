-- print the nthRandom number from nGens number of streams

module Main where

import Random.LFG (generators, Gen, step)
import Data.Char (isDigit)
import System (exitFailure)
import IO (hFlush, stdout)

main = do
    nthRandom <- getInt "Which position in the random sequence? "
    nGens <- getInt "How many generators? "
    let gens = take nGens generators
    mapM_ (print . genNth nthRandom) gens

genNth :: Int -> Gen -> Double
genNth 1 gen = fst $ step gen
genNth n gen = genNth (n-1) $ snd $ step gen

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
