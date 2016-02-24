module Parser where

import Data.List.Split
import Data.List
import Dominion

separate :: String -> [String]
separate str = filter (\el -> (length $ words el) > 0) $
               split (dropInitBlank $ dropFinalBlank $ oneOf "()") str

parseNotification :: String -> Notification
parseNotification tokens
    | isPrefixOf "moved" notif  = Moved notification
    | otherwise                = Move notification
        where sepTokens = separate tokens
              notification = parseState (drop 2 sepTokens) emptyState
              notif        = sepTokens!!1

parseState :: [String] -> State -> State
parseState [] state = state
parseState (token:rest) state
    | head parts == "players"  = parseState rest $ parsePlayers strippedParts state
    | head parts == "supply"   = parseState rest $ parseSupply strippedParts state
    | head parts == "trash"    = parseState rest $ parseTrash strippedParts state
    | head parts == "deck"     = parseState rest $ parseDeck strippedParts state
    | head parts == "hand"     = parseState rest $ parseHand strippedParts state
    | head parts == "discards" = parseState rest $ parseDiscards strippedParts state
    | head parts == "plays"    = parseState rest $ parsePlays strippedParts state
    | head parts == "actions"  = parseState rest $ parseActions (last parts) state
    | head parts == "buys"     = parseState rest $ parseBuys (last parts) state
    | head parts == "coins"    = parseState rest $ parseCoins (last parts) state
    | otherwise                = parseState rest state
        where parts = words token
              strippedParts = tail parts

parsePlayers :: [String] -> State -> State
parsePlayers players state = state { players = players }

parseActions :: String -> State -> State
parseActions count state = state { actionsLeft = (read count)::Int }

parseBuys :: String -> State -> State
parseBuys count state = state { buysLeft = (read count)::Int }

parseCoins :: String -> State -> State
parseCoins count state = state { coinsLeft = (read count)::Int }

parseSupply :: [String] -> State -> State
parseSupply cards state = state { supply = newSupply }
    where newSupply = parseCards cards

parseTrash :: [String] -> State -> State
parseTrash cards state = state { trash = newTrash }
    where newTrash = parseCards cards

parseDeck :: [String] -> State -> State
parseDeck cards state = state { deck = newDeck }
    where newDeck = parseCards cards

parseHand :: [String] -> State -> State
parseHand cards state = state { hand = newHand }
    where newHand = parseCards cards

parseDiscards :: [String] -> State -> State
parseDiscards cards state = state { discards = newDiscards }
    where newDiscards = parseCards cards

parsePlays :: [String] -> State -> State
parsePlays cards state = state { plays = newPlays }
    where newPlays = parseCards cards

parseCards :: [String] -> [Card]
parseCards cards = map stringToCard cards

stringToCard :: String -> Card
stringToCard "copper"   = Treasure Copper
stringToCard "silver"   = Treasure Silver
stringToCard "gold"     = Treasure Gold
stringToCard "estate"   = Victory Estate
stringToCard "duchy"    = Victory Duchy
stringToCard "province" = Victory Province
stringToCard "mine"     = Action Mine


emptyState = State {
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