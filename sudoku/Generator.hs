module Main(main) where

import Sudoku
import System.Random

main :: IO()
main = do
    gen <- getStdGen
    let b = generate gen
    putStrLn $ printUnsolved b
