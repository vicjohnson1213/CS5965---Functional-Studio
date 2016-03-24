module Lookups where

import Data

getCost :: Card -> Int

getCost (Treasure Copper)   = 0
getCost (Treasure Silver)   = 3
getCost (Treasure Gold)     = 6

getCost (Victory Estate)    = 2
getCost (Victory Duchy)     = 5
getCost (Victory Province)  = 8

getCost (Action Mine)       = 5
getCost (Action Cellar)     = 2
getCost (Action Market)     = 5
getCost (Action Remodel)    = 4
getCost (Action Smithy)     = 4
getCost (Action Village)    = 5
getCost (Action Woodcutter) = 5
getCost (Action Workshop)   = 3
getCost (Action Militia)    = 4
getCost (Action Moat)       = 2


buyPrecedence :: Card -> Int

buyPrecedence (Treasure Copper)   = 0
buyPrecedence (Treasure Silver)   = 1
buyPrecedence (Treasure Gold)     = 2

buyPrecedence (Victory Estate)    = 7
buyPrecedence (Victory Duchy)     = 8
buyPrecedence (Victory Province)  = 16

buyPrecedence (Action Mine)       = 9
buyPrecedence (Action Cellar)     = 2
buyPrecedence (Action Market)     = 15
buyPrecedence (Action Remodel)    = -1
buyPrecedence (Action Smithy)     = 14
buyPrecedence (Action Village)    = 10
buyPrecedence (Action Woodcutter) = 12
buyPrecedence (Action Workshop)   = 11
buyPrecedence (Action Militia)    = 13
buyPrecedence (Action Moat)       = 14

actPrecedence :: Card -> Int

actPrecedence (Action Mine)       = 4
actPrecedence (Action Cellar)     = 2
actPrecedence (Action Market)     = 8
actPrecedence (Action Remodel)    = -1
actPrecedence (Action Smithy)     = 5
actPrecedence (Action Village)    = 6
actPrecedence (Action Woodcutter) = 7
actPrecedence (Action Workshop)   = 3
actPrecedence (Action Militia)    = 9
actPrecedence (Action Moat)       = 1
actPrecedence _                   = error "Unknown action"

compareActPrecedence :: Card -> Card -> Ordering
compareActPrecedence c1 c2
    | actPrecedence c1 < actPrecedence c2 = GT
    | actPrecedence c1 > actPrecedence c2 = LT
    | otherwise                           = EQ

compareBuyPrecedence :: Card -> Card -> Ordering
compareBuyPrecedence c1 c2
    | buyPrecedence c1 < buyPrecedence c2 = GT
    | buyPrecedence c1 > buyPrecedence c2 = LT
    | otherwise                           = EQ


deckMax :: Card -> Int

deckMax (Treasure Copper)   = 100
deckMax (Treasure Silver)   = 100
deckMax (Treasure Gold)     = 100

deckMax (Victory Estate)    = 100
deckMax (Victory Duchy)     = 100
deckMax (Victory Province)  = 100

deckMax (Action Remodel)    = 0
deckMax _                   = 1

