main = do
    run

run = do
    line <- getLine
    print $ "Line: '" ++ line ++ "'"

    if line == "exit"
        then return line
        else run