import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.Tuple as T
import qualified System.Environment as ENV
import Data.Maybe (isJust, fromJust, isNothing)

type NGram = M.Map [String] Int

data Dictionary = Dictionary {
    n          :: Int,
    vocabulary :: [String],
    ngram      :: NGram
} deriving (Show)

-- | Specify the characters to keep
keepChars :: Char -> Bool
keepChars c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ". ")

-- | Remove characters that should be removed
removeBadChars :: String -> String
removeBadChars = filter keepChars

-- | Extends toLower to work on a whole string
strToLower :: String -> String
strToLower = map C.toLower

-- | Converts a atring to lower case and removes unwanted characters
fixString :: String -> String
fixString = strToLower . removeBadChars

-- | Returns true if the length of the list is equal to `co`
lenEq :: Int -> [a] -> Bool
lenEq co lst = length lst == co

-- | Generate groups of `co` words.  Turn each word to lower case and create the
--   groups. Finally remove any groups of words less than the desired NGram length.
getNGrams :: Int -> [String] -> [[String]]
getNGrams co strs = filter (lenEq co) $ map (take co) $ L.tails $ map strToLower strs

-- | Get the NGrams and add them to the Map, increasing counts if the NGram
--   already exists in the Map.
count :: Int -> [String] -> NGram
count co str = foldl (\acc el -> M.insertWith (+) el 1 acc) M.empty ngrams
    where ngrams = getNGrams co str

-- | Create the Dictionay with the n, vocabulary, and the NGram map
getCounts :: Int -> [String] -> Dictionary
getCounts co str = Dictionary co (vocab str) (count co str)

-- | Removes duplicate elements from the words of the corpus
vocab :: [String] -> [String]
vocab = S.toList . S.fromList

-- | Given NGrams and their counts, find the ones where the previous correct words
--    match the beginning of the NGram
filterResults :: Int -> [String] -> [String] -> Int -> Bool
filterResults co prev k _ = isPrefix
    where isPrefix = unwords (reverse . take (pred co) . reverse $ prev) `L.isPrefixOf` unwords k

-- | Since maps don't have a built in function to find the pair with the maximum
--   value, only the maximum key, this will swap the keys and values then find
--   the max.
maxVal :: NGram -> Maybe (Int, [String])
maxVal gram
    | (not . null) m    = Just $ M.findMax m
    | otherwise = Nothing
        where m = M.fromList $ map T.swap $ M.toList gram

-- | Take a Dictionary and a seed string, and give a guess for the next word.
--    If there is no match, remove the first word of the seed and try again.
makeGuess :: Dictionary -> [String] -> Maybe String
makeGuess _ []      = Nothing
makeGuess dict prev
    | isJust guess  = Just $ (!!min (length prev) (pred $ n dict)) $ snd $ fromJust guess
    | otherwise     = makeGuess dict $ reverse . drop 1 . reverse $ prev
        where guess = maxVal $ M.filterWithKey (filterResults (n dict) (map fixString prev)) $ ngram dict


-- predict :: Dictionary -> IO ()
-- predict dict = do
--     putStr "Word: "
--     input <- getLine
--     if input /= ""
--         then do
--             let guess = makeGuess dict $ words input
--             if isJust guess
--                 then do
--                     print $ fromJust guess
--                     predict dict
--                 else predict dict
--         else print "Adios"

runner :: Dictionary -> IO ()
runner dict = do
    putStr "Seed: "
    input <- getLine
    if input /= ""
        then do
            putStrLn $ runner' 100 (words input) dict
            putStrLn ""
            runner dict
        else print "Adios"


runner' :: Int -> [String] -> Dictionary -> String
runner' fuel sofar dict
    | fuel == 0 = unwords sofar
    | isNothing nextWord = unwords sofar
    | otherwise = runner' (pred fuel) (sofar ++ [fromJust nextWord]) dict
        where nextWord = makeGuess dict $ reverse . take (pred $ n dict) . reverse $ sofar

main :: IO ()
main = do
    args <- ENV.getArgs
    text <-  readFile $ head args

    let dict = getCounts 6 $ words . fixString $ text
    -- predict dict
    runner dict
    -- print $ firstChoice dict ["my"] 