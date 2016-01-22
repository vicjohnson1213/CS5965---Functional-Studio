module Main where

import Sudoku
import Data.Maybe
import System.Environment(getArgs)
import Network.HTTP
import Control.Applicative

get :: String -> IO String
get url = do
    response <- simpleHTTP $ getRequest url
    getResponseBody response

main = do
    args <- getArgs
    let addr = args!!0
    contents <- get addr
    let b = buildBoard (words contents)
    let sol = solve b
    putStrLn $ printBoard b
    if isNothing sol
        then do
            putStrLn "The board is illegal."
        else do
            putStrLn $ printBoard $ fromJust $ solve b