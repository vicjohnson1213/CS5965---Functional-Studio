{-# LANGUAGE ExistentialQuantification #-}

class Card_ a where
    cost :: a -> Int

class Valuable a where
    value :: a -> Int


data Treasure = Copper | Silver | Gold deriving (Eq, Ord, Enum, Show, Read)

instance Card_ Treasure where
    cost Copper = 0
    cost Silver = 3
    cost Gold   = 6

instance Valuable Treasure where
    value Copper = 1
    value Silver = 2
    value Gold   = 3


data Victory = Estate | Duchy | Province deriving (Eq, Ord, Enum, Show, Read)

instance Card_ Victory where
    cost Estate   = 2
    cost Duchy    = 5
    cost Province = 8

instance Valuable Victory where
    value Estate   = 1
    value Duchy    = 3
    value Province = 6

data Action = Mine deriving (Eq, Ord, Enum, Show, Read)

instance Card_ Action where
    cost Mine = 5


data Card = forall a. Card_ a => Card a
instance Card_ Card where
    cost (Card c) = cost c


getCost :: Card -> Int
getCost cd = cost cd

getVals :: [Card] -> [Int]
getVals cards = map cost cards

main = do
    print $ getVals [Card Copper, Card Silver, Card Estate]