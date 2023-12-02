{-# LANGUAGE OverloadedStrings #-}

import Data.Text (splitOn, pack, Text, unpack)

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

data Game = Game
    {
        idg :: Int,
        blue :: Int,
        red :: Int,
        green :: Int
    }
    deriving (Show)

parseGameNum :: Text -> Int
parseGameNum str = (splitOn " " str |> tail |> head) |> unpack |> read

parseSingleColor :: Text -> (Int, Int, Int)
parseSingleColor str =
    case color of
        "green" -> (0, 0, num)
        "red" -> (0, num, 0)
        "blue" -> (num, 0, 0)
        _ -> (0, 0, 0)
    where
        spl = splitOn " " str |> tail
        num = read (head spl |> unpack) :: Int
        color = tail spl |> head

sumColors :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sumColors (a, b, c) (a', b', c') = (a + a', b + b', c + c')

parseIntraGames lst = 
    lst
    |> map parseSingleColor
    |> foldr sumColors (0, 0, 0)

verify (a, b, c) = a <= 14 && b <= 12 && c <= 13

parseGame str id = parsed
    where
        parsed =
                str
                |> splitOn ";" -- [intra]
                |> map (splitOn ",") -- [[color, color, color]]
                |> map parseIntraGames -- [[(int, int, int)]
                |> map verify
                |> elem False
                

parseLine line = if games then 0 else gameId
    where
        game = splitOn ":" line
        gameId = parseGameNum (head game)
        games = parseGame (tail game |> head) gameId

filterGames :: Game -> Bool
filterGames game = if blue game <= 14 && red game <= 12 && green game <= 13 then True else False

sumGamesId :: Game -> Game -> Int
sumGamesId game game' = idg game + idg game'

parse lines =
    lines
    |> map parseLine
    |> sum

parseGame' str id = minBlue * minRed * minGreen
    where
        (blues, reds, greens) = 
                                str
                                |> splitOn ";" -- [intra]
                                |> map (splitOn ",") -- [[color, color, color]]
                                |> map parseIntraGames -- [[(int, int, int)]
                                |> unzip3
        minBlue = 
            blues
            |> maximum
        minRed = 
            reds
            |> maximum
        minGreen = 
            greens
            |> maximum

parseLine' line = games
    where
        game = splitOn ":" line
        gameId = parseGameNum (head game)
        games = parseGame' (tail game |> head) gameId

parse' lines = 
    lines
    |> map parseLine'
    |> sum

main :: IO()
main = do
    file <- readFile "day_02/input.txt"
    let splLines = lines file |> map pack
    putStr "Part 1: "
    print $ parse splLines
    putStr "Part 2: "
    print $ parse' splLines