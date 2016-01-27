module Main(main) where

import Sudoku
import System.Random
--import Data.List

--main = do
    --g <- newStdGen
    --let vals = randomRs (1, 9) g :: [Int]
    --g <- newStdGen
    --let idxs = randomRs (0, 80) g :: [Int]
    --let pairs = zip (take 400 vals) (take 400 idxs)
    --putStrLn $ printBoard $ generate pairs

--randInRng :: RandomGen g => g -> (Int, Int) -> (Int, g)
--randInRng gen rng = randomR rng gen

--newPair :: RandomGen g => g -> ((Int, Int), g)
--newPair gen = let (idx, gen') = randInRng gen (0,80)
--                  (val, gen'') = randInRng gen' (1,9)
--    in ((idx, val), gen'')

main :: IO()
main = do
    gen <- getStdGen
    let b = generate gen
    putStrLn $ printUnsolved b
