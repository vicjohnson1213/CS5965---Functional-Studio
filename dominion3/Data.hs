module Data where

data Treasure = Copper | Silver | Gold deriving (Show, Read, Eq)
data Victory  = Estate | Duchy | Province deriving (Show, Read, Eq)
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
                deriving (Show, Read, Eq)

data Card = Treasure Treasure |
            Victory Victory |
            Action Action
            deriving (Show, Read, Eq)

data Play = Act {
    action   :: Action,
    actCards :: [Card]
} | Add {
    addCard :: Card
} | Buy {
    buyCard :: Card
} | Clean {
    cleanCard :: Maybe Card
} deriving (Show, Read, Eq)

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
} | Attacked deriving (Show)

data Defense = MoatDefense | Discard {
    toDiscard :: [Card]
} deriving (Show)