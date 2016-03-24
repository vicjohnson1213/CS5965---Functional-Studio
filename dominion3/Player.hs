import Dominion
import Parser
import Data

import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    run
    

run :: IO ()
run = do
    line <- getLine

    if line /= ""
        then do
            let notification = parseNotification line

            case notification of
                (Move _)     -> do
                    print (move $ state notification)
                    run
                (Attacked _) -> do
                    print (defend $ state notification)
                    run
                _            -> run
        else
            run

--(move (state (actions 5) (buys 6) (coins 7) (players p1 p2 p3) (supply copper silver gold estate duchy province mine) (trash estate duchy mine) (deck copper mine gold estate) (deck copper) (discards mine) (plays copper silver mine) (hand silver gold mine)))