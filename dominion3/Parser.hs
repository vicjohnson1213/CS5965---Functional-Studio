module Parser where

import Data.List.Split
import Data.List
import Dominion()
import Data

separate :: String -> [String]
separate str = filter (not . null . words) $
               split (dropInitBlank $ dropFinalBlank $ oneOf "()") str

parseNotification :: String -> Notification
parseNotification tokens
    | "moved" `isPrefixOf` notif    = Moved st
    | "move" `isPrefixOf` notif     = Move st
    | "defended" `isPrefixOf` notif = Defended st
    | otherwise                     = Attacked st
        where sepTokens = separate tokens
              st = parseState (drop 2 sepTokens) emptyState
              notif        = sepTokens!!1

parseState :: [String] -> State -> State
parseState [] st = st
parseState (token:rest) st
    | head parts == "players"  = parseState rest $ parsePlayers strippedParts st
    | head parts == "supply"   = parseState rest $ parseSupply strippedParts st
    | head parts == "trash"    = parseState rest $ parseTrash strippedParts st
    | head parts == "deck"     = parseState rest $ parseDeck strippedParts st
    | head parts == "hand"     = parseState rest $ parseHand strippedParts st
    | head parts == "discards" = parseState rest $ parseDiscards strippedParts st
    | head parts == "plays"    = parseState rest $ parsePlays strippedParts st
    | head parts == "actions"  = parseState rest $ parseActions (last parts) st
    | head parts == "buys"     = parseState rest $ parseBuys (last parts) st
    | head parts == "coins"    = parseState rest $ parseCoins (last parts) st
    | otherwise                = parseState rest st
        where parts = words token
              strippedParts = tail parts

parsePlayers :: [String] -> State -> State
parsePlayers plyrs st = st { players = plyrs }

parseActions :: String -> State -> State
parseActions count st = st { actionsLeft = read count :: Int }

parseBuys :: String -> State -> State
parseBuys count st = st { buysLeft = read count :: Int }

parseCoins :: String -> State -> State
parseCoins count st = st { coinsLeft = read count :: Int }

parseSupply :: [String] -> State -> State
parseSupply cards st = st { supply = newSupply }
    where newSupply = parseCards cards

parseTrash :: [String] -> State -> State
parseTrash cards st = st { trash = newTrash }
    where newTrash = parseCards cards

parseDeck :: [String] -> State -> State
parseDeck cards st = st { deck = newDeck }
    where newDeck = parseCards cards

parseHand :: [String] -> State -> State
parseHand cards st = st { hand = newHand }
    where newHand = parseCards cards

parseDiscards :: [String] -> State -> State
parseDiscards cards st = st { discards = newDiscards }
    where newDiscards = parseCards cards

parsePlays :: [String] -> State -> State
parsePlays cards st = st { plays = newPlays }
    where newPlays = parseCards cards

parseCards :: ([String] -> [Card])
parseCards = map stringToCard

stringToCard :: String -> Card

stringToCard "copper"     = Treasure Copper
stringToCard "silver"     = Treasure Silver
stringToCard "gold"       = Treasure Gold

stringToCard "estate"     = Victory Estate
stringToCard "duchy"      = Victory Duchy
stringToCard "province"   = Victory Province

stringToCard "mine"       = Action Mine
stringToCard "cellar"     = Action Cellar
stringToCard "market"     = Action Market
stringToCard "remodel"    = Action Remodel
stringToCard "smithy"     = Action Smithy
stringToCard "village"    = Action Village
stringToCard "woodcutter" = Action Woodcutter
stringToCard "workshop"   = Action Workshop
stringToCard "militia"    = Action Militia
stringToCard "moat"       = Action Moat
stringToCard _            = error "Unknown Card"

emptyState :: State
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