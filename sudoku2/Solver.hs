import Sudoku
import System.Environment (getArgs)

main :: IO()
main = do
    contents <- readFile $ "boards/sudoku.txt"
    --contents <- readFile $ "boards/small.txt"
    --contents <- readFile $ "boards/hard.txt"
    let bd = buildBoard contents
    let newBd = fillObviousChoices bd
    let newBd' = findOnlyOptions bd
    putStrLn "ORIGINAL:"
    print bd
    putStrLn "\n"
    putStrLn "\n"
    putStrLn "fillObviousChoices:"
    print newBd
    putStrLn "\n"
    putStrLn "\n"
    putStrLn "findOnlyOptions:"
    print newBd'
