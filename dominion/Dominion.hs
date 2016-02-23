import Data.List.Split

data Card = Treasure {
    cost  :: Int,
    value :: Int
} | Victory {
    cost :: Int,
    points :: Int
} | Action {
    cost :: Int
} deriving (Show, Read, Eq)

copper = Treasure { cost = 0, value = 1 }
silver = Treasure { cost = 3, value = 2 }
gold   = Treasure { cost = 6, value = 3 }

estate   = Victory { cost = 2, points = 1 }
duchy    = Victory { cost = 5, points = 3 }
province = Victory { cost = 8, points = 6 }

mine = Action { cost = 5 }

data State = State {
    actions  :: Int,
    buys     :: Int,
    coins    :: Int,
    players  :: [String],
    deck     :: [Card],
    discards :: [Card],
    hand     :: [Card],
    plays    :: [Card],
    supply   :: [Card],
    trash    :: [Card]
} deriving (Show, Read)

testState = State {
    actions = 1,
    buys = 1,
    coins = 1,
    players = ["Test1", "Test2", "Test3"],
    deck = [copper, copper, copper, estate, estate, mine],
    discards = [duchy, estate, copper],
    hand = [copper, copper, copper, silver, estate],
    plays = [],
    supply = [mine, mine, mine],
    trash = []
}

-- parseCard :: String -> Card
-- parseCard "copper"   = copper
-- parseCard "silver"   = silver
-- parseCard "gold"     = gold
-- parseCard "estate"   = estate
-- parseCard "duchy"    = duchy
-- parseCard "province" = province
-- parseCard "mine"     = mine
-- parseCard c          = error $ "Not a valid card: " ++ show c

-- parseState :: String -> [String]
-- parseState str = words str

main = do
    -- print $ map parseCard ["copper", "province", "gold", "duchy"]
    print $ testState