module Sudoku where

import Data.List
import Data.List.Split
import Data.Char

data Cell = Cell {
    orig :: Bool,
    val :: Int
}

instance Show Cell where
    show (Cell or v) = "(" ++ show v ++ ", " ++ show or ++ ")"

instance Eq Cell where
    left == right = val left == val right

data Board = Board {
    width :: Int,
    height :: Int,
    cells :: [Cell]
}

instance Show Board where
    show (Board w h cls) = intercalate "\n\n" $
                          map (intercalate "\n") $
                          chunksOf h $
                          map (intersperse ' ') $
                          map (intercalate " ") $
                          map (chunksOf w) $
                          chunksOf (w^2) $
                          map (intToDigit . val) cls

mkCell :: String -> Cell
mkCell "_" = Cell False 0
mkCell val = Cell True (read val :: Int)

buildBoard :: String -> Board
buildBoard str = Board fw fh cls
    where w:h:rst = words str
          fw = read w :: Int
          fh = read h :: Int
          cls = map mkCell rst

getRows :: Board -> [[Cell]]
getRows (Board w _ cls) = chunksOf (w^2) cls

getRow :: Int -> Board -> [Cell]
getRow r bd = (getRows bd)!!r

getCol :: Int -> Board -> [Cell]
getCol c bd = map (!!c) $ getRows bd

getGroup :: (Int, Int) -> Board -> [Cell]
getGroup (x, y) bd@(Board w h _) = concat $
                                   map (!!newX) $
                                   map (chunksOf w) $
                                   (chunksOf h $ getRows bd)!!newY
    where newX = x `div` w
          newY = y `div` h

getCell :: (Int, Int) -> Board -> Cell
getCell cds@(x, y) bd@(Board w _ cls) = cls!!idx
    where idx = coordsToIdx cds bd

setCell :: (Int, Int) -> Cell -> Board -> Board
setCell cds cell bd@(Board w h cls) = newBd
    where idx = coordsToIdx cds bd
          (left, _:right) = splitAt idx $ cells bd
          newBd = Board w h (left ++ [cell] ++ right)

coordsToIdx :: (Int, Int) -> Board -> Int
coordsToIdx (x, y) (Board w h _) = y * w^2 + x

idxToCoords :: Int -> Board -> (Int, Int)
idxToCoords idx (Board w _ _) = (idx `mod` w^2, idx `div` w^2)

validExistingCell :: (Int, Int) -> Board -> Bool
validExistingCell cds bd = validCell cds cell bd 3
    where cell = getCell cds bd

validNewCell :: (Int, Int) -> Cell -> Board -> Bool
validNewCell cds cell bd = validCell cds cell bd 0

validCell :: (Int, Int) -> Cell -> Board -> Int -> Bool
validCell cds@(x, y) cell bd count = valCount == count
    where valCount = length . filter (==cell) $
                     concat [getRow y bd, getCol x bd, getGroup cds bd]

countSolved :: Board -> Int
countSolved (Board _ _ cls) = length $ filter (/=empty) cls
    where empty = Cell False 0

possibilitiesForCell :: (Int, Int) -> Board -> [Cell]
possibilitiesForCell cds@(x, y) bd@(Board w h cls)
    | orig cell = [cell]
    | otherwise = [Cell False c | c <- [1..w*h], validNewCell cds (Cell False c) bd]
        where cell = getCell cds bd

possibilitiesForCellsInRow :: Int -> Board -> [[Cell]]
possibilitiesForCellsInRow row bd@(Board w _ _) = map (\c -> possibilitiesForCell c bd) [(x, row) | x <- [0..w^2 - 1]]

possibilitiesForCellsInCol :: Int -> Board -> [[Cell]]
possibilitiesForCellsInCol col bd@(Board _ h _) = map (\c -> possibilitiesForCell c bd) [(col, y) | y <- [0..h^2 - 1]]

removeNth :: Int -> [a] -> [a]
removeNth _ [] = []
removeNth idx lst = fst ++ snd
    where (fst, _:snd) = splitAt idx lst