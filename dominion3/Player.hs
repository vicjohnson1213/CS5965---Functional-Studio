import Dominion
import Parser

main = run

run = do
    line <- getLine
    let notification = parseNotification line
    -- print $ move $ state notification
    -- run
    if isMove notification
        then do
            print $ move $ state notification
            run
        else run

--(move (state (actions 5) (buys 6) (coins 7) (players p1 p2 p3) (supply copper silver gold estate duchy province mine) (trash estate duchy mine) (deck copper mine gold estate) (deck copper) (discards mine) (plays copper silver mine) (hand silver gold mine)))