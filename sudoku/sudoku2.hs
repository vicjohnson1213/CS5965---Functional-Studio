import System.IO
import Data.List
import Data.Maybe

data Cell = Cell {
    value :: Int,
    orig :: Bool
} deriving (Show)

data Board = Board {
    width :: Int,
    height :: Int,
    cells :: [Cell]
} deriving (Show)

main = do
    contents <- readFile "sudoku.txt"
    --contents <- readFile "bad.txt"
    --contents <- readFile "small.txt"
    let all = map convertEl (words contents)
    let b = Board (value (head all)) (value (head . tail $ all)) (tail . tail $ all)
    4

convertEl :: String -> Square
convertEl "_" = Square 0 False
convertEl x = Square (readInt x) True

readInt :: String -> Int
readInt = read