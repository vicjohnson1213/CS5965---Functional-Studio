import Data.List.Split
import Data.Maybe

data Treasure = Copper | Silver | Gold deriving (Show, Read, Eq, Ord)
data Victory  = Estate | Duchy | Province deriving (Show, Read, Eq, Ord)
data Action   = Mine deriving (Show, Read, Eq)

data Card = Treasure Treasure |
            Victory Victory |
            Action Action
            deriving (Show, Read, Eq)

data Play = Act {
    action :: Action,
    from   :: Treasure,
    to     :: Treasure
} | Add {
    addCard :: Treasure
} | Buy {
    buyCard :: Card
} | Clean {
    cleanCard :: Maybe Card
}deriving (Show, Read, Eq)

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

getCost :: Card -> Int
getCost (Treasure Copper)  = 0
getCost (Treasure Silver)  = 3
getCost (Treasure Gold)    = 6
getCost (Victory Estate)   = 2
getCost (Victory Duchy)    = 5
getCost (Victory Province) = 8
getCost (Action Mine)      = 5

hasCard :: Card -> [Card] -> Bool
hasCard card cards = any (== card) cards

cardCount :: Card -> [Card] -> Int
cardCount card cards = length $ filter (== card) cards

toTreasure :: Card -> Maybe Treasure
toTreasure (Treasure tres) = Just $ tres
toTreasure _               = Nothing

isTreasure :: Card -> Bool
isTreasure (Treasure _) = True
isTreasure _            = False

hasTreasure :: [Card] -> Bool
hasTreasure [] = False
hasTreasure (x:xs)
    | isTreasure x = True
    | otherwise    = hasTreasure xs

getTreasure :: [Card] -> [Treasure]
getTreasure cards = map (fromJust . toTreasure) $ filter isTreasure cards

chooseMineCard :: State -> Maybe (Treasure, Treasure)
chooseMineCard state
    | silverInHand && goldInSupply   = Just (Silver, Gold)
    | copperInHand && silverInSupply = Just (Copper, Silver)
    | otherwise                      = Nothing
        where copperInHand   = hasCard (Treasure Copper) (hand state)
              silverInHand   = hasCard (Treasure Silver) (hand state)
              silverInSupply = hasCard (Treasure Silver) (supply state)
              goldInSupply   = hasCard (Treasure Gold) (supply state)

tryAction :: State -> Maybe Play
tryAction state
    | actionsLeft state == 0 = Nothing
    | isNothing chosenCards  = Nothing
    | otherwise              = Just $ Act Mine (fst $ fromJust chosenCards)
                                               (snd $ fromJust chosenCards)
        where chosenCards = chooseMineCard state

chooseBuyCard :: State -> Maybe Play
chooseBuyCard state
    | coinsLeft state >= 8 && provinceInSupply = Just $ Buy $ Victory Province
    | coinsLeft state < 5 && copperInSupply    = Just $ Buy $ Treasure Copper
    | minesInDeck < 2 && minesInSupply         = Just $ Buy $ Action Mine
    | duchiesInSupply                          = Just $ Buy $ Victory Duchy
    | otherwise                                = Just $ Buy $ Treasure Copper
        where provinceInSupply = hasCard (Victory Province) $ supply state
              duchiesInSupply  = hasCard (Victory Duchy) $ supply state
              copperInSupply   = hasCard (Treasure Copper) $ supply state
              minesInSupply    = hasCard (Action Mine) $ supply state
              minesInDeck      = (cardCount (Action Mine) $ deck state) +
                                 (cardCount (Action Mine) $ hand state) +
                                 (cardCount (Action Mine) $ discards state)

tryBuy :: State -> Maybe Play
tryBuy state
    | buysLeft state == 0      = Nothing
    | hasTreasure $ hand state = Just $ Add $ (head . getTreasure . hand) state
    | otherwise                = chooseBuyCard state


cleanHand :: State -> Play
cleanHand state
    | (length . hand) state > 0 = Clean $ (Just . head . hand) state
    | otherwise                 = Clean Nothing

move :: State -> Play
move state
    | not $ isNothing act = fromJust act
    | not $ isNothing buy = fromJust buy
    | otherwise         = clean
        where act   = tryAction state
              buy   = tryBuy state
              clean = cleanHand state

main = do
    print $ move testState
    -- print $ fromJust $ tryAction testState

testState = State {
    actionsLeft  = 1,
    buysLeft     = 0,
    coinsLeft    = 1,
    players      = ["Test1", "Test2", "Test3"],
    deck         = [],
    discards     = [],
    hand         = [Action Mine],
    supply       = [Treasure Gold, Treasure Silver, Victory Province, Action Mine],
    plays        = [],
    trash        = []
}