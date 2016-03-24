module Data where

showCards :: [Card] -> String
showCards = concatMap show

data Treasure = Copper | Silver | Gold deriving (Read, Eq)
data Victory  = Estate | Duchy | Province deriving (Read, Eq)
data Action   = Mine 
              | Cellar
              | Market
              | Remodel
              | Smithy
              | Village
              | Woodcutter
              | Workshop
              | Militia
              | Moat
                deriving (Read, Eq)

instance Show Treasure where
    show Copper = " copper"
    show Silver = " silver"
    show Gold   = " gold"

instance Show Victory where
    show Estate   = " estate"
    show Duchy    = " duchy"
    show Province = " province"

instance Show Action where
    show Mine       = " mine"
    show Cellar     = " cellar"
    show Market     = " market"
    show Remodel    = " remodel"
    show Smithy     = " smithy"
    show Village    = " village"
    show Woodcutter = " woodcutter"
    show Workshop   = " workshop"
    show Militia    = " militia"
    show Moat       = " moat"

data Card = Treasure Treasure |
            Victory Victory |
            Action Action
            deriving (Read, Eq)

instance Show Card where
    show (Treasure val) = show val
    show (Victory val)  = show val
    show (Action val)   = show val

data Play = Act {
    action   :: Action,
    actCards :: [Card]
} | Add {
    addCard :: Card
} | Buy {
    buyCard :: Card
} | Clean {
    cleanCard :: [Card]
} deriving (Read, Eq)

instance Show Play where
    show (Act act cards) = "(act" ++ show act ++ showCards cards ++ ")"
    show (Add card)      = "(add" ++ show card ++ ")"
    show (Buy card)      = "(buy" ++ show card ++ ")"
    show (Clean card)    = "(clean" ++ showCards card ++")"

data State = State {
    actionsLeft :: Int,
    buysLeft    :: Int,
    coinsLeft   :: Int,
    players     :: [String],
    deck        :: [Card],
    discards    :: [Card],
    hand        :: [Card],
    plays       :: [Card],
    supply      :: [Card],
    trash       :: [Card]
} deriving (Show, Read)

data Notification = Move {
    state :: State
} | Moved {
    state :: State
} | Attacked {
    state :: State
}  | Defended {
    state :: State
} deriving (Show)

data Defense = MoatDefense | Discard {
    toDiscard :: [Card]
}

instance Show Defense where
    show MoatDefense = "(moat)"
    show (Discard cards) = "(discard" ++ showCards cards ++ ")"