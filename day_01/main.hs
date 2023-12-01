import Data.Char (isDigit)
import Data.List (subsequences, isPrefixOf, sortBy)
import Data.Maybe (isNothing, fromJust, isJust)

validString = ["one", "two", "three", "four", 
    "five", "six", "seven", "eight", "nine"]

validString' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

mapStrToNum str = 
    case str of
        "one" -> "1"
        "two" -> "2"
        "three" -> "3"
        "four" -> "4"
        "five" -> "5"
        "six" -> "6"
        "seven" -> "7"
        "eight" -> "8"
        "nine" -> "9"


(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

parseLine :: String -> Int
parseLine line = read totalNum
    where
        firstNum = 
            line 
            |> dropWhile (not . isDigit) 
            |> head
        lastNum = 
            line 
            |> reverse 
            |> dropWhile (not . isDigit) 
            |> head
        totalNum = firstNum : lastNum : ""

parse :: [String] -> Int
parse lines =
    lines
    |> map parseLine
    |> sum

-- Return Just the position of the search string in line string or Nothing
subStr :: (Num t, Eq a) => [a] -> [a] -> t -> Maybe t
subStr search line acc = 
    case line of 
        [] -> Nothing
        l -> 
            if length l >= length search
            then if lox then Just acc else subStr search (tail line) (acc + 1)
            else Nothing
    where
        lox = search `isPrefixOf` line

containR :: Eq a => [a] -> [a] -> Int -> [Maybe ([a], Int)]
containR line search plus
    | length line < length search = []
    | isNothing isContained = []
    | otherwise = Just (search, isContainedN + plus) : containR (drop (isContainedN + (length search)) line) search (plus + isContainedN + (length search))
        where
            isContained = subStr search line 0 :: Maybe Int
            isContainedN = fromJust isContained

contain :: Eq a => [a] -> [a] -> [Maybe ([a], Int)]
contain line search = containR line search 0

containList :: Eq a => [[a]] -> [a] -> [([a], Int)]
containList valid line = 
    valid
    |> map (contain line)
    |> concat
    |> filter isJust
    |> map fromJust
    |> sortBy (\(_, b) (_, b') -> compare b b')

parseLine' :: [Char] -> Int
parseLine' line = read final
    where
        strings = containList validString line
        nums = containList validString' line
        concat = (strings ++ nums) |> sortBy (\(_, b) (_, b') -> compare b b')
        firstNum = if head concat |> fst |> length == 1
                    then head concat |> fst
                    else head concat |> fst |> mapStrToNum 
        lastNum = if last concat |> fst |> length == 1
                    then last concat |> fst
                    else last concat |> fst |> mapStrToNum 
        final = firstNum ++ lastNum

parse' :: [String] -> Int
parse' lines =
    lines
    |> map parseLine'
    |> sum

main :: IO ()
main = do
    file <- readFile "day_01/input.txt"
    let splLines = lines file
    putStr "First part: "
    print $ parse splLines
    putStr "Second part: "
    print $ parse' splLines