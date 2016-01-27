module Sudoku (
    Cell(..),
    Board(..),
    buildBoard,
    printBoard,
    solve,
    generate
) where

import Data.List
import Data.Char
import Data.Maybe
import System.Random

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

-- | printBoard converts a board into a more readable form.
printBoard :: Board -> String
printBoard b = printBoard' (toVals b) ((width b)^2)

-- | printBoard' is a helper to make a board more readable.
printBoard' :: [Int] -> Int -> String
printBoard' [] _ = ""
printBoard' i s = let (x, xs) = splitAt s i
    in (intersperse ' ' (map intToDigit x)) ++ "\n" ++ (printBoard' xs s)

-- | toVals weeds out all board data besides the number in each cell.
toVals :: Board -> [Int]
toVals s = map (\s -> (value s)) (cells s)

-- | getRow returns the row of the board at a given index.
getRow :: Int -> Board -> [Cell]
getRow n b = take ((width b) * (width b)) (drop ((n) * (width b) * (width b)) (cells b))

-- | getRows returns a list of all rows in the board
getRows :: Board -> [[Cell]]
getRows b = [getRow x b | x <- [0..((height b) * (height b) - 1)]]

-- | getCol returns the column of the board at a given index.
getCol :: Int -> Board -> [Cell]
getCol n b = [(cells b)!!x | x <- take ((height b) * (height b)) [n, n + (width b) * (width b) ..]]

-- | getCols returns a list of all columns in the board
getCols :: Board -> [[Cell]]
getCols b = [getCol x b | x <- [0..((width b) * (width b) - 1)]]

-- | getGroup returns group of the board at a given set of coordinates.
getGroup :: (Int, Int) -> Board -> [Cell]
getGroup (x, y) b = let rows = getRows b
    in concat (map (\r -> take (width b) (drop ((width b) * x) r))
                   (take (height b) (drop ((height b) * y) rows)))

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

-- | validBoard checks whether a board is legal.
validBoard :: Board -> Bool
validBoard b = validBoard' b 0

-- | validBoard' checks whether a board is legal.
validBoard' :: Board -> Int -> Bool
validBoard' b s = let coords = idxToCoords s b
    in if s >= (length $ cells b)
        then True
        else if (value $ getCell coords b) == 0
            then validBoard' b (succ s)
            else (validCell coords b) && validBoard' b (succ s)


-- | solve returns the solved board.
solve :: Board -> Maybe Board
solve b = let solution = solve' b (-1)
    in if validBoard b && (status solution) == Complete
        then Just $ solution
        else Nothing

-- | solve' iterates each cell and returns the resulting board, whether it is
--   broken or not.
solve' :: Board -> Int -> Board
solve' b s = let nextBd = solveSq b (succ s)
    in case status nextBd of
        Incomplete -> solve' nextBd (succ s)
        Complete -> nextBd
        Broken -> if (pred s) < -1
            then Board 0 0 [] Broken
            else solve' nextBd (pred s)

-- | solveSq finds the next legal value for a specific cell and returns a new
--   board with that cell updated.
solveSq :: Board -> Int -> Board
solveSq b s = let coords = idxToCoords s b
                  sq = getCell coords b
                  newSq = Cell (succ $ value sq) False
                  newBd = setCell coords newSq b
    in if original sq
        then if s == (length $ cells b) - 1
            then setStatus b Complete
            else b
        else if validCell coords newBd
            then if s == (length $ cells newBd) - 1
                then setStatus newBd Complete
                else setStatus newBd Incomplete
            else if value newSq > ((width newBd) * (height newBd)) - 1
                then setCell coords (Cell 0 False)
                    (setStatus newBd Broken)
                else solveSq newBd s



-- | generate creates a new random sudoku board.
generate :: [(Int, Int)] -> Board
generate l = let emptyBd = (map (\e -> Cell 0 False) [1..81])
                 bd = generate' l (Board 3 3 emptyBd Incomplete) 25
    in if isNothing (solve bd)
        then Board 0 0 emptyBd Broken
        else bd

-- | generate' attempts to put a new value in a random cell.
generate' :: [(Int, Int)] -> Board -> Int -> Board
generate' v b f = let (val,idx) = head v
                      newCell = Cell val True
                      newBd = setCell (idxToCoords idx b) newCell b
    in if f == 0
        then b
        else if validBoard newBd
            then generate' (tail v) newBd (pred f)
            else generate' (tail v) b f
