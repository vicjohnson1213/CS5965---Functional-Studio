import Sudoku
import System.Environment (getArgs)

main :: IO()
main = do
    contents <- readFile $ "boards/custom.txt"
    -- contents <- readFile $ "boards/sudoku.txt"
    --contents <- readFile $ "boards/small.txt"
    --contents <- readFile $ "boards/hard.txt"
    let bd = buildBoard contents
    print bd
    putStrLn "\n"

    print $ possibilitiesForCellsInCol 3 bd