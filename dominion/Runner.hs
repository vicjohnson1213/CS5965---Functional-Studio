main = do
    run

run = do
    line <- getLine
    print $ words $ tail . init $ line

    if line == "exit"
        then return line
        else run