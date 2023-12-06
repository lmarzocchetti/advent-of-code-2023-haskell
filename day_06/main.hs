{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, unpack)

data Race = Race {
    duration :: Int,
    record :: Int
}
    deriving (Show)

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

toInt :: [String] -> [Int]
toInt = map read

readInt :: String -> Int
readInt = read

toRace :: (Int, Int) -> Race
toRace (a, b) = Race {duration = a, record = b}

parseLine' l =
    l
    |> splitOn ":"
    |> (head . tail)
    |> splitOn " "
    |> filter (/="")
    |> map unpack

parseLine = toInt . parseLine'

countWays :: Race -> Int
countWays Race {duration, record} = length possib
    where
        possib =
            [x*(duration - x) | x <- [0..duration]]
            |> filter (> record)

parse file = 
    races
    |> map countWays
    |> product
    where
        time = head file |> parseLine
        distance = (head . tail) file |> parseLine
        races = zip time distance |> map toRace

parse' file = race |> countWays
    where
        time = head file |> parseLine' |> concat |> readInt 
        distance = (head . tail) file |> parseLine' |> concat |> readInt
        race = toRace (time, distance)

main :: IO ()
main = do
    file <- readFile "day_06/input.txt"
    let splLines = lines file |> map pack
    putStr "Part 1: "
    print $ parse splLines
    putStr "Part 2: "
    print $ parse' splLines
