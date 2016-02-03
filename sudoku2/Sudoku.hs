module Sudoku (
    Cell(..),
    Board(..),
    toVals,
    buildBoard,
    printUnsolved,
    printBoard,
    possibilitiesForCell,
    possibilitiesForCellRow,
    possibilitiesForCellCol,
    --possibilitiesForCellGroup,
    fillAutomatic,
    initialFill,
    countUnsolved,
    getGroupIdxs
) where

import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe

-- | The status of the board.
data Status = Complete | Incomplete | Broken deriving (Show, Eq)

-- | A Cell represents a single cell of the board.
data Cell = Cell {
    value :: Int,
    original :: Bool
} deriving (Show)

-- | A board represents the relevant information of the Sudoku board.
data Board = Board {
    width :: Int,
    height :: Int,
    cells :: [Cell],
    status :: Status
} deriving (Show)

-- | buildBoard creates a new board from a list of characters describing the board.
buildBoard :: [String] -> Board
buildBoard vs = let w:h:s = map convertEl vs
    in Board w h (map buildCell s) Incomplete

-- | buildCell creates a single cell from an integer
buildCell :: Int -> Cell
buildCell 0 = Cell 0 False
buildCell v = Cell v True

-- | convertEl turns a character to it's corresponding number, handling 
--   underscores appropriately.
convertEl :: String -> Int
convertEl "_" = 0
convertEl x = read x

cellToString :: Char -> Char
cellToString ' ' = ' '
cellToString '0' = '_'
cellToString x = x

printUnsolved :: Board -> String
printUnsolved bd = "3 3\n" ++ (map cellToString (printBoard bd))

-- | printBoard converts a board into a more readable form.
printBoard :: Board -> String
printBoard b = printBoard' (toVals (cells b)) ((width b)^2)

-- | printBoard' is a helper to make a board more readable.
printBoard' :: [Int] -> Int -> String
printBoard' [] _ = ""
printBoard' i s = let (x, xs) = splitAt s i
    in (intersperse ' ' (map intToDigit x)) ++ "\n" ++ (printBoard' xs s)

-- | toVals weeds out all board data besides the number in each cell.
toVals :: [Cell] -> [Int]
toVals s = map value s

-- | getRow returns the row of the board at a given index.
getRow :: Int -> Board -> [Cell]
getRow n b = (chunksOf 9 (cells b))!!n

-- | getRows returns a list of all rows in the board
getRows :: Board -> [[Cell]]
getRows b = chunksOf 9 (cells b)

-- | getCol returns the column of the board at a given index.
getCol :: Int -> Board -> [Cell]
getCol n b = [(cells b)!!x | x <- take ((height b) * (height b)) [n, n + (width b) * (width b) ..]]

-- | getCols returns a list of all columns in the board
getCols :: Board -> [[Cell]]
getCols b = [getCol x b | x <- [0..((width b) * (width b) - 1)]]

-- | getGroup returns group of the board at a given set of coordinates.
getGroup :: (Int, Int) -> Board -> [Cell]
getGroup crds b = let rows = getRows b
    in map (\c -> getCell c b) (getGroupIdxs crds b)

getGroupIdxs :: (Int, Int) -> Board -> [(Int, Int)]
getGroupIdxs (x, y) b = let allCoords = chunksOf 9 [(x, y) | y <- [0..((height b)^2 - 1)],
                                                             x <- [0..((width b)^2 - 1)]]
    in concat [r!!x | r <- map (chunksOf 3)
                        [allCoords!!x | x <- [(y * (height b))..((y * (height b)) + 2)]]]

-- | coordToIdx converts a set of coordinates to a list index for the board.
coordToIdx :: (Int, Int) -> Board -> Int
coordToIdx (x, y) b = y * (width b) * (width b) + x

-- | idxToCoords converts a list index into a set of coordinates.
idxToCoords :: Int -> Board -> (Int, Int)
idxToCoords i b = (mod i ((width b) * (width b)), div i ((width b) * (width b)))

-- | getCell returns the cell at a particular set of coordinates.
getCell :: (Int, Int) -> Board -> Cell
getCell c b = (cells b)!!(coordToIdx c b)

-- | setCell updates the cell at a particular set of coordinates and returns the
--   new board.
setCell :: (Int, Int) -> Cell -> Board -> Board
setCell c s b = let (left,_:right) = splitAt (coordToIdx c b) (cells b)
    in Board (width b) (height b) (left ++ (s:right)) (status b)

-- | setStatus returns a new board with a given status
setStatus :: Board -> Status -> Board
setStatus b s = Board (width b) (height b) (cells b) s

-- | validCell checks whether a cell value is legal per the rules of sudoku.
validCell :: (Int, Int) -> Board -> Bool
validCell (x, y) b = let sq = (cells b)!!(coordToIdx (x, y) b)
    in ((value sq) >= 1 && (value sq) <= (width b) * (height b))
        && notElem (value sq) (delete (value sq) 
            (map (\s -> (value s)) (getGroup ((div x (height b)), (div y (width b))) b)))
                && notElem (value sq) (delete (value sq) (map (\s -> (value s)) (getRow y b)))
                && notElem (value sq) (delete (value sq) (map (\s -> (value s)) (getCol x b)))



possibilitiesForCell :: (Int, Int) -> Board -> [Cell]
possibilitiesForCell c bd = let idx = coordToIdx c
    in possibilitiesForCell' c bd 1 []

possibilitiesForCell' :: (Int, Int) -> Board -> Int -> [Cell] -> [Cell]
possibilitiesForCell' c bd val ps = let oldCell = getCell c bd
                                        newCell = Cell val False
                                        newBd = setCell c newCell bd
    in if (value oldCell) /= 0
        then [oldCell]
        else if val > 9
            then ps
            else if validCell c newBd
                then possibilitiesForCell' c bd (succ val) (ps ++ [newCell])
                else possibilitiesForCell' c bd (succ val) ps

possibilitiesForCellRow :: Int -> Board -> [[Cell]]
possibilitiesForCellRow y bd = map (\x -> possibilitiesForCell (x, y) bd) [0..(((width bd)^2) - 1)]

possibilitiesForCellCol :: Int -> Board -> [[Cell]]
possibilitiesForCellCol x bd = map (\y -> possibilitiesForCell (x, y) bd) [0..(((height bd)^2) - 1)]

possibilitiesForCellGroup :: (Int, Int) -> Board -> [[Cell]]
possibilitiesForCellGroup c bd = map (\g -> possibilitiesForCell g bd) (getGroupIdxs c bd)

fillAutomatic :: Board -> Board
fillAutomatic bd = fillAutomatic' bd 0

fillAutomatic' :: Board -> Int -> Board
fillAutomatic' bd idx = let coords = idxToCoords idx bd
                            poss = possibilitiesForCell coords bd
    in if idx >= (length $ cells bd)
        then bd
        else if length poss == 1
            then fillAutomatic' (setCell coords (head poss) bd) (succ idx)
            else fillAutomatic' bd (succ idx)

countUnsolved :: Board -> Int
countUnsolved bd = countUnsolved' 0 0 bd

countUnsolved' :: Int -> Int -> Board -> Int
countUnsolved' count idx bd = let cell = getCell (idxToCoords idx bd) bd
    in if idx >= (length $ cells bd)
        then count
        else if (value cell) == 0
            then countUnsolved' (succ count) (succ idx) bd
            else countUnsolved' count (succ idx) bd

--guessCell :: (Int, Int) -> Board -> Board

initialFill :: Board -> Board
initialFill bd = initialFill' (countUnsolved bd) bd

initialFill' :: Int -> Board -> Board
initialFill' prev bd = let newBd = fillAutomatic bd
                           newCount = countUnsolved newBd
    in if newCount == prev
        then bd
        else initialFill' newCount newBd

