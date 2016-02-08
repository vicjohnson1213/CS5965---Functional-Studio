import Sudoku
import System.Environment (getArgs)

main :: IO()
main = do
    --contents <- readFile $ "boards/sudoku.txt"
    contents <- readFile $ "boards/small.txt"
    --contents <- readFile $ "boards/hard.txt"
    let bd = buildBoard contents
    let newBd = fillObviousChoices bd
    print bd
    putStrLn "\n"
    print $ fillObviousChoices newBd
