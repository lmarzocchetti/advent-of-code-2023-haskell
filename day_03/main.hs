{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, split, unpack)
import qualified Data.Text as Tx
import Text.Read (readMaybe, read)
import Data.Char (isDigit)
import Data.Maybe (isNothing, isJust)
import Debug.Trace
import Numeric.Natural

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

data Elem = Elem {
    val :: String,
    pos :: (Int, Int)
}
    deriving (Show)

newtype Gear = Gear {
    valG :: [Int]
}

splitCluster :: [Char] -> Int -> [Elem]
splitCluster "" _ = []
splitCluster str position
    | len == 0 = Elem {val = [head str], pos = (position, position + 1)} : splitCluster (tail str) (position + 1)
    | otherwise = Elem {val = taken, pos = (position, position + len)} : splitCluster (drop len str) (position + len)
    where
        taken = takeWhile isDigit str
        len = length taken

parseLine' :: Tx.Text -> Int -> [Elem]
parseLine' "" _ = []
parseLine' str position
    | len == 0 = parseLine' (Tx.tail str) (position + 1)
    | otherwise = 
        case (readMaybe taken :: Maybe Natural) of
            Just x -> Elem {val = taken, pos = (position, position + len)} : parseLine' (Tx.drop len str) (position + len)
            Nothing -> splitCluster taken position ++ parseLine' (Tx.drop len str) (position + len)
    where
        taken = Tx.takeWhile (/= '.') str |> unpack
        len = length taken

verifyRange :: (Int, Int) -> (Int, Int) -> Bool
verifyRange (a, b) (a', b') = (a >= a' && a <= b') || (b >= a' && b <= b') || (a >= a' - 1 && a <= b' - 1)

verifyElem :: [Elem] -> Elem -> Bool
verifyElem ver lst = if length filtered == 0 then False else True
    where
        filtered =
            ver
            |> filter (\x -> isNothing (readMaybe (val x) :: Maybe Natural)) 
            |> filter (\x -> verifyRange (pos x) (pos lst))

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

containNum :: [Elem] -> Bool
containNum l = 
    not $ l
    |> filter (\x -> isJust (readMaybe (val x) :: Maybe Natural))
    |> isEmpty

findElem :: [[Elem]] -> Int -> [Elem]
findElem x now = 
    if length x /= now 
    then if canCheck 
        then actual ++ findElem x (now + 1) 
        else findElem x (now + 1)
    else []
    where
        current = (x !! now) |> filter (\x -> isJust (readMaybe (val x) :: Maybe Natural))
        currentVer = (x !! now) |> filter (\x -> isNothing (readMaybe (val x) :: Maybe Natural))
        prev = if now == 0 then [] else (x !! (now - 1))
        next = if now == (length x - 1) then [] else (x !! (now + 1))
        canCheck = containNum current
        toVerify = prev ++ next ++ currentVer
        actual = current |> filter (verifyElem toVerify)

findNeigh :: [Elem] -> Elem -> Gear
findNeigh ver lst = Gear {valG = allVal}
    where
        filtered =
            ver
            |> filter (\x -> isJust (readMaybe (val x) :: Maybe Natural)) 
            |> filter (\x -> verifyRange (pos x) (pos lst))
        allVal = filtered |> map val |> toInt

findGear :: [[Elem]] -> Int -> [Gear]
findGear x now = if length x /= now then actual ++ (findGear x (now + 1)) else []
    where
        current = (x !! now) |> filter (\x -> isJust (readMaybe (val x) :: Maybe Natural))
        prev = if now == 0 then [] else (x !! (now - 1))
        next = if now == (length x - 1) then [] else (x !! (now + 1))
        currentStar = (x !! now) |> filter (\x -> isNothing (readMaybe (val x) :: Maybe Natural)) |> filter (\x -> val x == "*")
        toVerify = prev ++ next ++ current
        actual = currentStar |> map (findNeigh toVerify)


parseLine :: Tx.Text -> [Elem]
parseLine str = parseLine' str 0

toInt :: [String] -> [Int]
toInt = map read

parse :: [Tx.Text] -> Int
parse file = verified |> toInt |> sum
    where
        parsed = file |> map parseLine
        verified = findElem parsed 0 |> map val

parse' :: [Tx.Text] -> Int
parse' file = 
    verified 
    |> map valG 
    |> map product
    |> sum
    where
        parsed = file |> map parseLine
        verified = findGear parsed 0 |> filter (\x -> length (valG x) == 2)

main :: IO ()
main = do
    file <- readFile "day_03/input.txt"
    let splLines = lines file |> map pack
    putStr "Part 1: "
    print $ parse splLines
    putStr "Part 2: "
    print $ parse' splLines