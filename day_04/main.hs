{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, unpack)
import Data.List (intersect)

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

data Card = Card {
    winNums :: [Int],
    ourNums :: [Int],
    copies :: Int
}
    deriving (Show)

toInt :: [String] -> [Int]
toInt = map read

cardValue :: Card -> Int
cardValue Card {winNums, ourNums, copies} = if howMuch > 0 then 2^(howMuch - 1) else 0
    where
        howMuch = winNums `intersect` ourNums |> length

parseLine line = Card {winNums = winnerText, ourNums = ourText, copies = 1}
    where
        noHeader = splitOn ":" line |> tail |> head
        winnerText = splitOn "|" noHeader |> head |> splitOn " " |> filter (/= "") |> map unpack |> toInt
        ourText = splitOn "|" noHeader |> tail |> head |> splitOn " " |> filter (/= "") |> map unpack |> toInt

parse file =
    file
    |> map parseLine
    |> map cardValue
    |> sum

addCards :: [Card] -> Int -> Int -> [Card]
addCards [] _ _ = []
addCards xs 0 _ = xs
addCards (Card {winNums, ourNums, copies}:xs) left howMuch = 
    Card {winNums, ourNums, copies = copies + howMuch} : addCards xs (left - 1) howMuch

processCopies :: [Card] -> [Card]
processCopies [] = []
processCopies (Card {winNums, ourNums, copies}:xs) = Card {winNums, ourNums, copies} : processCopies augmented
    where
        luckNums = winNums `intersect` ourNums |> length
        augmented = addCards xs luckNums copies

parse' file =
    processed
    |> map copies
    |> sum
    where
        processed = file |> map parseLine |> processCopies

main :: IO ()
main = do
    file <- readFile "day_04/input.txt"
    let splLines = lines file |> map pack
    putStr "Part 1: "
    print $ parse splLines
    putStr "Part 2: "
    print $ parse' splLines