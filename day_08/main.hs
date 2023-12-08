{-# LANGUAGE OverloadedStrings#-}
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Text (splitOn, pack, Text, unpack)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

parseTuple :: Text -> (String, String)
parseTuple str = (splitted !! 0, splitted !! 1)
    where
        splitted =
            str
            |> T.tail |> T.reverse |> T.tail |> T.reverse
            |> splitOn ", "
            |> map unpack

parseNetwork :: [Text] -> Map.Map String (String, String)
parseNetwork istr = Map.fromList (zip from to)
    where
        splitted =
            istr
            |> map (splitOn " = ")
            |> map (\[x, y] -> (x, y))
            |> unzip

        from = fst splitted |> map unpack
        to = snd splitted |> map parseTuple

mapLR :: Char -> (a, a) -> a
mapLR 'L' = fst
mapLR 'R' = snd

navigate :: Map.Map String (String, String) -> String -> Int -> String -> Int -> Int
navigate nav istr poss posg count
    | poss == length istr = navigate nav istr 0 posg count
    | retrieve == "ZZZ" = count + 1
    | otherwise = navigate nav istr (poss + 1) retrieve (count + 1)
    where
        lr = (istr !! poss) |> mapLR
        retrieve = nav ! posg |> lr

parse file = navigate network directions 0 "AAA" 0
    where
        directions = head file
        network = parseNetwork (tail file |> map pack)

allEndsWith :: [String] -> Char -> Bool
allEndsWith xs ch =
    xs
    |> map last
    |> all (==ch)

myMap :: Map.Map String (String, String)
{-# NOINLINE myMap #-}
myMap = unsafePerformIO $ do
    file <- readFile "day_08/input.txt"
    let splLines = lines file |> filter (/= "")
    let network = parseNetwork (tail splLines |> map pack)
    return network

getNbSteps :: [Char] -> String -> Int
getNbSteps (instruction:instructions) node
    | (== 'Z') $ node !! 2 = 0
    | otherwise = (+1) $ getNbSteps instructions next
    where next = (if instruction == 'L' then fst else snd) . ((!) myMap) $ node

parse' file = foldr lcm 1 costs
    where
        directions = head file
        getKeys = (filter (\x -> x !! 2 == 'A')) . Map.keys
        costs = map (getNbSteps (cycle directions)) . getKeys $ myMap

main :: IO ()
main = do
    file <- readFile "day_08/input.txt"
    let splLines = lines file |> filter (/= "")
    putStr "Part 1: "
    print $ parse splLines
    putStr "Part 2: "
    print $ parse' splLines