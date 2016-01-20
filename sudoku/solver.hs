import System.IO
import Data.List
import Data.Maybe
import Debug.Trace

data Status = Complete | Incomplete | Broken deriving (Show, Eq)

data Square = Square {
    value :: Int,
    solved :: Bool
} deriving (Show)

data Board = Board {
    width :: Int,
    height :: Int,
    squares :: [Square],
    status :: Status
} deriving (Show)

main = do
    --contents <- readFile "sudoku.txt"
    contents <- readFile "bad.txt"
    --contents <- readFile "small.txt"
    let all = map convertEl (words contents)
    let b = Board (value (head all)) (value (head . tail $ all)) (tail . tail $ all) Incomplete
    print b
    --print $ toVals $ squares $ b
    --print $ toVals $ squares $ solve b


convertEl :: String -> Square
convertEl "_" = Square 0 False
convertEl x = Square (readInt x) True

readInt :: String -> Int
readInt = read

toVals :: [Square] -> [Int]
toVals s = map (\s -> (value s)) s

-- BEGIN GETTERS FOR BOARD

getRow :: Int -> Board -> [Square]
getRow n b = take ((width b) * (width b)) (drop ((n) * (width b) * (width b)) (squares b))

getRows :: Board -> [[Square]]
getRows b = [getRow x b | x <- [0..((height b) * (height b) - 1)]]

getCol :: Int -> Board -> [Square]
getCol n b = [(squares b)!!x | x <- take ((height b) * (height b)) [n, n + (width b) * (width b) ..]]
--getCol n b = map (\r -> r!!n) (getRows b)

getCols :: Board -> [[Square]]
getCols b = [getCol x b | x <- [0..((width b) * (width b) - 1)]]

getGroup :: (Int, Int) -> Board -> [Square]
getGroup (x, y) b = let rows = getRows b
    in concat (map (\r -> take (width b) (drop ((width b) * x) r)) (take (height b) (drop ((height b) * y) rows)))

getListIdx :: (Int, Int) -> Board -> Int
getListIdx (x, y) b = y * (width b) * (width b) + x

getCoords :: Int -> Board -> (Int, Int)
getCoords i b = (mod i ((width b) * (width b)), div i ((width b) * (width b)))

getSquare :: (Int, Int) -> Board -> Square
getSquare c b = (squares b)!!(getListIdx c b)
-- END GETTERS FOR BOARD

-- BEGIN SETTERS FOR BOARD

setSquare :: (Int, Int) -> Square -> Board -> Board
setSquare c s b = setSquareIdx (getListIdx c b) s b

--setSquareIdx :: Int -> Square -> Board -> Board
--setSquareIdx x s b = let (left,_:right) = splitAt x (squares b)
--    in (Board (width b) (height b) (left ++ s:right))


setSquareIdx :: Int -> Square -> Board -> Board
setSquareIdx x s b = let (left,_:right) = splitAt x (squares b)
    in Board (width b) (height b) (left ++ (s:right)) (status b)

-- END SETTERS FOR BOARD

validSquare :: (Int, Int) -> Board -> Bool
validSquare (x, y) b = let sq = (squares b)!!(getListIdx (x, y) b)
    in ((value sq) >= 1 && (value sq) <= (width b) * (height b))
        && notElem (value sq) (delete (value sq) 
            (map (\s -> (value s)) (getGroup ((div x (height b)), (div y (width b))) b)))
                && notElem (value sq) (delete (value sq) (map (\s -> (value s)) (getRow y b)))
                && notElem (value sq) (delete (value sq) (map (\s -> (value s)) (getCol x b)))


solve :: Board -> Board
solve b = solve' b 0

solve' :: Board -> Int -> Board
solve' b s = let oldBd = b
                 newBd = (solveSquare oldBd s)
    in if (status newBd) == Broken
        then solve' newBd (pred s)
        else if (status newBd) == Complete
            then newBd
            else solve' newBd (succ s)

solveSquare :: Board -> Int -> Board
solveSquare b s = let coords = getCoords s b
                      sq = getSquare coords b
                      newSq = Square (succ (value sq)) (solved sq)
                      newBd = setSquare coords newSq b
    in if solved sq
        then b
        else if validSquare coords newBd
            then if s >= (length $ squares b) - 1
                then Board (width newBd) (height newBd) (squares newBd) Complete
                else newBd
            else if value newSq > (width b) * (height b)
                then setSquare coords (Square 0 False) (Board (width newBd) (height newBd) (squares newBd) Broken)
                else solveSquare newBd s