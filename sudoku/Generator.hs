module Main(main) where

import Sudoku
import System.Random
import Data.List

main = do
    g <- newStdGen
    let vals = randomRs (1, 9) g :: [Int]
    g <- newStdGen
    let idxs = randomRs (0, 80) g :: [Int]
    let pairs = zip (take 400 vals) (take 400 idxs)
    putStrLn $ printBoard $ generate pairs