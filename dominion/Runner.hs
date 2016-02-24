import Dominion

main = do
    putStrLn ""
    print $ move cleanState
    print $ move cleanCardState
    -- print $ move mineState
    -- print $ move addCopperState
    -- print $ move buyProvinceState
    -- print $ move buyMineState
    -- print $ move buyDuchyState

cleanState = State {
    actionsLeft  = 0,
    buysLeft     = 0,
    coinsLeft    = 0,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [],
    supply       = [],
    plays        = [],
    trash        = []
}

cleanCardState = State {
    actionsLeft  = 0,
    buysLeft     = 0,
    coinsLeft    = 0,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [Treasure Copper],
    supply       = [],
    plays        = [],
    trash        = []
}

mineState = State {
    actionsLeft  = 1,
    buysLeft     = 0,
    coinsLeft    = 0,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [Action Mine, Treasure Copper, Treasure Silver],
    supply       = [Treasure Silver, Treasure Gold],
    plays        = [],
    trash        = []
}

addCopperState = State {
    actionsLeft  = 0,
    buysLeft     = 1,
    coinsLeft    = 0,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [],
    supply       = [],
    plays        = [],
    trash        = []
}

buyProvinceState = State {
    actionsLeft  = 0,
    buysLeft     = 1,
    coinsLeft    = 8,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [],
    supply       = [Victory Estate, Victory Duchy, Victory Province],
    plays        = [],
    trash        = []
}

buyMineState = State {
    actionsLeft  = 0,
    buysLeft     = 1,
    coinsLeft    = 6,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [Action Mine],
    supply       = [Action Mine, Victory Estate, Victory Duchy, Victory Province],
    plays        = [],
    trash        = []
}

buyDuchyState = State {
    actionsLeft  = 0,
    buysLeft     = 1,
    coinsLeft    = 6,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [Action Mine, Action Mine],
    supply       = [Action Mine, Victory Estate, Victory Duchy, Victory Province],
    plays        = [],
    trash        = []
}