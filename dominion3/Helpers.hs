module Helpers where

import Data
import Lookups

isMove :: Notification -> Bool
isMove (Move _) = True
isMove _        = False

-- | Check to see if a specified set of cards contains a certian card
hasCard :: (Card -> [Card] -> Bool)
hasCard = elem

-- | Count the number of a certain card in a set of cards
cardCount :: Card -> [Card] -> Int
cardCount card cards = length $ filter (== card) cards

cardCountInDeck :: Card -> State -> Int
cardCountInDeck card st = cardCount card $ hand st ++
                                              deck st ++
                                              discards st

-- | Check whether or not a given card is a Treasure card
isTreasure :: Card -> Bool
isTreasure (Treasure _) = True
isTreasure _            = False

-- | Check whether or not a set of cards has a Treasure card in it
hasTreasure :: [Card] -> Bool
hasTreasure = any isTreasure

-- | Filter out any non-treasure cards from a set of cards
getTreasure :: [Card] -> [Card]
getTreasure = filter isTreasure



isVictory :: Card -> Bool
isVictory (Victory _) = True
isVictory _           = False

hasVictory :: [Card] -> Bool
hasVictory = any isVictory



isAction :: Card -> Bool
isAction (Action _) = True
isAction _          = False

filterAction :: Card -> Bool
filterAction card = isAction card && actPrecedence card > 0

extractAction :: Card -> Action
extractAction (Action ac) = ac
extractAction _           = error "Not an action"



filterByCost :: Int -> Card -> Bool
filterByCost cost card = getCost card <= cost

filterTooMany :: State -> Card -> Bool
filterTooMany st card = cardCountInDeck card st < deckMax card