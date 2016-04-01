import Prelude hiding (Word)
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Maybe
import System.Environment

type Count = Int
type Word = String
type UsedWords = Map.Map Word Count
type WordDict = Map.Map Word UsedWords

insertWord :: WordDict -> Word -> Word -> WordDict
insertWord dict word foll = Map.insertWith updateWord lword (Map.singleton lfoll 1) dict
    where lword = map Char.toLower word
          lfoll = map Char.toLower foll
          updateWord _ = Map.insertWith (+) lfoll 1

learnWords :: WordDict -> String -> WordDict
learnWords dict []  = dict
learnWords dict str = fst $ fst $ List.mapAccumL accumulator (dict, head $ words str) $ tail $ words str

accumulator :: (WordDict, Word) -> Word -> ((WordDict, Word), Word)
accumulator dict val = ((uncurry insertWord dict val, val), val)

learnStrings :: WordDict -> [String] -> WordDict
learnStrings dict []   = dict
learnStrings dict strs = learnStrings (learnWords dict (head strs)) $ tail strs

guessWord :: WordDict -> Word -> Maybe Word
guessWord dict word
    | Map.member word dict = Just $ fst $ Map.findMax (fromJust $ Map.lookup lword dict) 
    | otherwise            = Nothing
        where lword = map Char.toLower word

predict :: WordDict -> IO ()
predict dict = do
    putStr "Word: "
    word <- getLine
    if word /= ""
        then do
            let guess = guessWord dict word
            if isJust guess
                then do
                    print $ fromJust guess
                    predict dict
                else predict dict
        else print "Adios"

isAlpha :: Char -> Bool
isAlpha c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ". ")

replace :: Char -> Char -> String -> String
replace o r = map repl
    where repl c = if c == o then r else c

runner :: Int -> [String] -> WordDict -> String
runner fuel sofar dict
    | fuel == 0 = unwords sofar
    | isNothing nextWord = unwords sofar
    | otherwise = runner (pred fuel) (sofar ++ [fromJust nextWord]) dict
        where nextWord = guessWord dict $ last sofar

teach :: IO ()
teach = do
    -- line <- getLine
    args <- getArgs
    text <-  readFile $ head args
    let filtered = filter isAlpha $ replace '\n' ' ' text
        dict     = learnStrings Map.empty $ map (unwords . words) $ Split.endBy "." filtered
    -- print filtered
    -- print dict
    print $ runner 20 ["down"] dict
    predict dict
    -- if filtered /= "-END TRAINING-"
    --     then teach $ learnStrings dict $ map (unwords . words) $ Split.endBy "." filtered
    --     else predict dict

main :: IO ()
main = teach