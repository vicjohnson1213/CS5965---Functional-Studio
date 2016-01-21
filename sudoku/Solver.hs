module Main(main) where

import System.IO
import System.Environment(getArgs)
import Sudoku
import Data.Maybe

main = do
    args <- getArgs
    let filename = args!!0
    contents <- readFile filename
    let all = words contents
    let b = buildBoard all
    --print b
    let sol = solve b
    putStrLn $ printBoard b
    if isNothing sol
        then do
            putStrLn "The board is illegal."
        else do
            putStrLn $ printBoard $ fromJust $ solve b