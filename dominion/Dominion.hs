import Data.List.Split
import Data.Maybe

data Treasure = Copper | Silver | Gold deriving (Show, Read, Eq, Ord)
data Victory  = Estate | Duchy | Province deriving (Show, Read, Eq, Ord)
data Action   = Mine deriving (Show, Read, Eq)

data Card = Treasure Treasure | Victory Victory | Action Action deriving (Show, Read, Eq)

data Play = Act {
    action :: Action,
    from   :: Treasure,
    to     :: Treasure
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

hasCopper :: State -> Bool
hasCopper state = any (== Treasure Copper) $ hand state

hasSilver :: State -> Bool
hasSilver state = any (== Treasure Silver) $ hand state

hasMine :: State -> Bool
hasMine state = any (== Action Mine) $ hand state

chooseMineCard :: State -> Maybe (Treasure, Treasure)
chooseMineCard state
    | hasSilver state = Just (Silver, Gold)
    | hasCopper state = Just (Copper, Silver)
    | otherwise       = Nothing

tryAction :: State -> Maybe Play
tryAction state
    | actionsLeft state == 0 = Nothing
    | isNothing chosenCards  = Nothing
    | otherwise              = Just $ Act Mine (fst $ fromJust chosenCards) (snd $ fromJust chosenCards)
        where chosenCards = chooseMineCard state

main = do
    print $ fromJust $ tryAction testState

testState = State {
    actionsLeft  = 1,
    buysLeft     = 1,
    coinsLeft    = 1,
    players      = ["Test1", "Test2", "Test3"],
    deck         = [],
    discards     = [],
    hand         = [Treasure Copper, Treasure Silver, Action Mine, Victory Estate, Victory Duchy],
    supply       = [Treasure Gold],
    plays        = [],
    trash        = []
}