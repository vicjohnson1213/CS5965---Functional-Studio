module Dominion where

import Data.List
import Data.List.Split()
import Data.Maybe

import Data
import Lookups
import Helpers

-- | Determine whether or not a mine can happen, if it can, then choose whether
    -- to mine from Copper -> Silver or Silver -> Gold
chooseMineCards :: State -> Maybe (Card, Card)
chooseMineCards st
    | hasMine && silverInHand && goldInSupply   = Just (Treasure Silver, Treasure Gold)
    | hasMine && copperInHand && silverInSupply = Just (Treasure Copper, Treasure Silver)
    | otherwise                                 = Nothing
        where hasMine        = hasCard (Action Mine) $ hand st
              copperInHand   = hasCard (Treasure Copper) $ hand st
              silverInHand   = hasCard (Treasure Silver) $ hand st
              silverInSupply = hasCard (Treasure Silver) $ supply st
              goldInSupply   = hasCard (Treasure Gold) $ supply st

chooseCellarCards :: State -> [Card]
chooseCellarCards st = filter isVictory $ hand st

chooseWorkshopCard :: State -> Maybe Card
chooseWorkshopCard st = Just $ minimumBy compareBuyPrecedence $ filter (filterByCost 4) $ supply st

chooseAction :: State -> Maybe Action
chooseAction st
    | null $ hand st = Nothing
    | null actions   = Nothing
    | otherwise      = Just $ extractAction $ maximumBy compareActPrecedence $ filter filterAction $ hand st
        where actions = filter filterAction $ hand st

buildAction :: State -> Action -> Maybe Play
buildAction st Cellar   = Just $ Act Cellar $ chooseCellarCards st
buildAction st Workshop
    | isJust chosenCard = Just $ Act Workshop [fromJust chosenCard]
    | otherwise         = Nothing
        where chosenCard = chooseWorkshopCard st
buildAction st Mine
    | isJust chosenCards = Just $ Act Mine [fst $ fromJust chosenCards,
                                            snd $ fromJust chosenCards]
    | otherwise          = Nothing
        where chosenCards = chooseMineCards st
buildAction _ act = Just $ Act act []


-- | Try to perform an action, if there are no more actions, then do nothing, or
    -- if no actions are valid, then do nothing
tryAction :: State -> Maybe Play
tryAction st
    | actionsLeft st == 0    = Nothing
    | isNothing chosenAction = Nothing
    | isNothing builtAction  = Nothing
    | otherwise              = Just $ fromJust builtAction
        where chosenAction = chooseAction st
              builtAction  = buildAction st (fromJust chosenAction)

-- | Determine which cards to buy.  These are a relatively arbitrary set of rules,
    -- so no guarantee on how this will perform
chooseBuyCard :: State -> Maybe Play
chooseBuyCard st
    | null $ supply st = Nothing
    | null buyCards    = Nothing
    | shouldBuyCopper  = Just $ Buy $ Treasure Copper
    | otherwise        = Just $ Buy $ maximumBy compareBuyPrecedence buyCards
        where shouldBuyCopper = coinsLeft st == 0 && hasCard (Treasure Copper) (supply st)
              sup             = filter (filterByCost $ coinsLeft st) $ supply st
              buyCards        = filter (filterTooMany st) sup

-- | Determine whether or not the player is able to buy a card, if the player can,
    -- then choose which one to buy.
tryBuy :: State -> Maybe Play
tryBuy st
    | buysLeft st == 0      = Nothing
    | hasTreasure $ hand st = Just $ Add $ (head . getTreasure . hand) st
    | otherwise             = chooseBuyCard st

-- Clean the hand with either a card to display or nothing if the hand is empty
cleanHand :: State -> Play
cleanHand st
    | (not . null . hand) st = Clean [(head . hand) st]
    | otherwise                 = Clean []

-- | Actually perform the move and return the desired play
move :: State -> Play
move st
    | isJust act = fromJust act
    | isJust buy = fromJust buy
    | otherwise           = clean
        where act   = tryAction st
              buy   = tryBuy st
              clean = cleanHand st

chooseDiscardCards :: State -> [Card]
chooseDiscardCards st = take (length (hand st) - 3) (sortBy compareDiscardPrecedence $ hand st)

defend :: State -> Defense
defend st
    | hasCard (Action Moat) (hand st) = MoatDefense
    | otherwise                       = Discard $ chooseDiscardCards st