module Dominion where

import Data.List.Split
import Data.Maybe

data Treasure = Copper | Silver | Gold deriving (Show, Read, Eq)
data Victory  = Estate | Duchy | Province deriving (Show, Read, Eq)
data Action   = Mine deriving (Show, Read, Eq)

data Card = Treasure Treasure |
            Victory Victory |
            Action Action
            deriving (Show, Read, Eq)

data Play = Act {
    action :: Action,
    from   :: Card,
    to     :: Card
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
} deriving (Show)

getCost :: Card -> Int
getCost (Treasure Copper)  = 0
getCost (Treasure Silver)  = 3
getCost (Treasure Gold)    = 6
getCost (Victory Estate)   = 2
getCost (Victory Duchy)    = 5
getCost (Victory Province) = 8
getCost (Action Mine)      = 5

isMove :: Notification -> Bool
isMove (Move _) = True
isMove _        = False

-- | Check to see if a specified set of cards contains a certian card
hasCard :: Card -> [Card] -> Bool
hasCard card cards = any (== card) cards

-- | Count the number of a certain card in a set of cards
cardCount :: Card -> [Card] -> Int
cardCount card cards = length $ filter (== card) cards

-- | Check whether or not a given card is a Treasure card
isTreasure :: Card -> Bool
isTreasure (Treasure _) = True
isTreasure _            = False

-- | Check whether or not a set of cards has a Treasure card in it
hasTreasure :: [Card] -> Bool
hasTreasure cards = any isTreasure cards

-- | Filter out any non-treasure cards from a set of cards
getTreasure :: [Card] -> [Card]
getTreasure cards = filter isTreasure cards

-- | Determine whether or not a mine can happen, if it can, then choose whether
    -- to mine from Copper -> Silver or Silver -> Gold
chooseMineCard :: State -> Maybe (Card, Card)
chooseMineCard state
    | hasMine && silverInHand && goldInSupply   = Just (Treasure Silver, Treasure Gold)
    | hasMine && copperInHand && silverInSupply = Just (Treasure Copper, Treasure Silver)
    | otherwise                                 = Nothing
        where hasMine        = hasCard (Action Mine) $ hand state
              copperInHand   = hasCard (Treasure Copper) $ hand state
              silverInHand   = hasCard (Treasure Silver) $ hand state
              silverInSupply = hasCard (Treasure Silver) $ supply state
              goldInSupply   = hasCard (Treasure Gold) $ supply state

-- check for mine card

-- | Try to perform an action, if there are no more actions, then do nothing, or
    -- if no actions are valid, then do nothing
tryAction :: State -> Maybe Play
tryAction state
    | actionsLeft state == 0 = Nothing
    | isNothing chosenCards  = Nothing
    | otherwise              = Just $ Act Mine (fst $ fromJust chosenCards)
                                               (snd $ fromJust chosenCards)
        where chosenCards = chooseMineCard state

-- | Determine which cards to buy.  These are a relatively arbitrary set of rules,
    -- so no guarantee on how this will perform
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

-- | Determine whether or not the player is able to buy a card, if the player can,
    -- then choose which one to buy.
tryBuy :: State -> Maybe Play
tryBuy state
    | buysLeft state == 0      = Nothing
    | hasTreasure $ hand state = Just $ Add $ (head . getTreasure . hand) state
    | otherwise                = chooseBuyCard state

-- Clean the hand with either a card to display or nothing if the hand is empty
cleanHand :: State -> Play
cleanHand state
    | (length . hand) state > 0 = Clean $ (Just . head . hand) state
    | otherwise                 = Clean Nothing

-- | Actually perform the move and return the desired play
move :: State -> Play
move state
    | not $ isNothing act = fromJust act
    | not $ isNothing buy = fromJust buy
    | otherwise           = clean
        where act   = tryAction state
              buy   = tryBuy state
              clean = cleanHand state