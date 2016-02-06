import Sudoku
import System.Environment (getArgs)

main :: IO()
main = do
    --contents <- readFile $ "boards/sudoku.txt"
    contents <- readFile $ "boards/small.txt"
    --contents <- readFile $ "boards/hard.txt"
    let bd = buildBoard contents
    print bd
    putStrLn "\n"
    --print $ getGroup (0,0) bd
    putStrLn "\n"
    print $ possibilitiesForCellsInGroup (0,0) bd
