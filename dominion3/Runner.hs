import Dominion
import Data

main :: IO ()
main = do
    putStrLn ""
    -- print $ move cleanState
    -- print $ move cleanCardState
    -- print $ move mineState
    -- print $ move addCopperState
    -- print $ move buyProvinceState
    -- print $ move noMineState
    -- print $ move buyMineState
    print $ defend newthing

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
    supply       = [Treasure Copper],
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

noMineState = State {
    actionsLeft  = 1,
    buysLeft     = 0,
    coinsLeft    = 0,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [Treasure Copper, Treasure Silver],
    supply       = [Action Mine, Treasure Gold],
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

newthing = State {
    actionsLeft  = 0,
    buysLeft     = 1,
    coinsLeft    = 6,
    players      = [],
    deck         = [],
    discards     = [],
    hand         = [Action Mine, Action Mine, Action Mine, Victory Estate, Victory Duchy, Victory Duchy],
    supply       = [Action Mine, Victory Estate, Victory Duchy, Victory Province],
    plays        = [],
    trash        = []
}