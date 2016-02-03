import Sudoku
import System.Environment (getArgs)

main :: IO()
main = do
    contents <- readFile $ "boards/sudoku.txt"
    --contents <- readFile $ "boards/small.txt"
    --contents <- readFile $ "boards/hard.txt"
    let bd = buildBoard $ words contents
    let sbd = initialFill bd
    --putStrLn $ printBoard $ fillAutomatic $ fillAutomatic bd
    --print $ countUnsolved $ fillAutomatic $ fillAutomatic bd
    --putStrLn $ printBoard sbd
    --print $ map toVals (possibilitiesForCellGroup (0,0) sbd)
    print $ getGroupIdxs (0,0) sbd