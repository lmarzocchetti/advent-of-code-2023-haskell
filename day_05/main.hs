{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, pack, unpack, chunksOf)
import Data.Bifunctor ( Bifunctor(first) )

data MapRange = MapRange {
    destStart :: Int,
    sourceStart :: Int,
    rangeLen :: Int
}
    deriving (Show)

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

toInt :: [String] -> [Int]
toInt = map read

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = f xs [] -- the initial aggregator
    where f [] agg = [agg]
          f (y : ys) agg = if p y
                           then agg : f ys [] -- we are ignoring the element here
                           else f ys (agg ++ [y]) -- put y into the aggregator

parseMapRange :: String -> MapRange
parseMapRange s = MapRange {destStart = a, sourceStart = b, rangeLen = c}
    where
        (a, b, c) = s |> pack |> splitOn " " |> map unpack |> toInt |> tuplify3

parseMapRangeL :: [String] -> [MapRange]
parseMapRangeL l = l |> map parseMapRange

inRange :: Int -> MapRange -> Bool
inRange x MapRange {destStart, sourceStart, rangeLen} =
    x >= sourceStart && x < (sourceStart + rangeLen)

calculateNum :: Maybe MapRange -> Int -> Int
calculateNum (Just MapRange {destStart, sourceStart, rangeLen}) x =
    destStart + (x - sourceStart)
calculateNum Nothing x = x

mapSeed :: [[MapRange]] -> Int -> Int
mapSeed mapping seed = location
    where
        seedToSoilA = mapping !! 0 |> filter (inRange seed)
        seedToSoil = if null seedToSoilA then Nothing else Just (head seedToSoilA)
        soil = calculateNum seedToSoil seed

        soilToFertilizerA = mapping !! 1 |> filter (inRange soil)
        soilToFertilizer = if null soilToFertilizerA then Nothing else Just (head soilToFertilizerA)
        fertilizer = calculateNum soilToFertilizer soil

        fertilizerToWaterA = mapping !! 2 |> filter (inRange fertilizer)
        fertilizerToWater = if null fertilizerToWaterA then Nothing else Just (head fertilizerToWaterA)
        water = calculateNum fertilizerToWater fertilizer

        waterToLightA = mapping !! 3 |> filter (inRange water)
        waterToLight = if null waterToLightA then Nothing else Just (head waterToLightA)
        light = calculateNum waterToLight water

        lightToTemperatureA = mapping !! 4 |> filter (inRange light)
        lightToTemperature = if null lightToTemperatureA then Nothing else Just (head lightToTemperatureA)
        temperature = calculateNum lightToTemperature light

        temperatureToHumidityA = mapping !! 5 |> filter (inRange temperature)
        temperatureToHumidity = if null temperatureToHumidityA then Nothing else Just (head temperatureToHumidityA)
        humidity = calculateNum temperatureToHumidity temperature

        humidityToLocationA = mapping !! 6 |> filter (inRange humidity)
        humidityToLocation = if null humidityToLocationA then Nothing else Just (head humidityToLocationA)
        location = calculateNum humidityToLocation humidity

parse lines = seeds |> map (mapSeed mapping) |> minimum
    where
        splitted = lines |> splitWhen (== "")
        seeds = splitted |> head |> head
            |> pack |> splitOn ":" |> tail |> head
            |> splitOn " " |> filter (/= "") |> map unpack |> toInt

        mapping = splitted |> tail |> map tail |> map parseMapRangeL

ranges :: (Bool, (Int, Int)) -> MapRange -> [(Bool, (Int, Int))]
ranges n@(True, _) _ = pure n
ranges (_, (s1, len)) (MapRange {destStart, sourceStart = s2, rangeLen})
    | s1 < s2 && e1 > e2 = [(True, (s2 + d, rangeLen)), (False, (s1, s2 - s1)), (False, (e2 + 1, len - rangeLen - (s2 - s1)))]
    | s1 >= s2 && e1 <= e2 = pure (True, (s1 + d, len))
    | e1 < s2 || s1 > e2 = pure (False, (s1, len))
    | s1 >= s2 = [(True, (s1 + d, o1)), (False, (s1 + o1, len - o1))]
    | otherwise = [(False, (s1, len - o2)), (True, (s1 + len - o2 + d, o2))]
    where
        e1 = s1 + len - 1
        e2 = s2 + rangeLen - 1
        d = destStart - s2
        o1 = e2 - s1 + 1
        o2 = e1 - s2 + 1

rangeFolder :: (Bool, (Int, Int)) -> [MapRange] -> [(Bool, (Int, Int))]
rangeFolder r = map (first (const False)) . foldl (\acc m -> acc >>= (`ranges` m)) [r]

readInt :: String -> Int
readInt = read

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

parse' lines = minimum $ map (fst . snd) $ foldl func seeds mapping
    where
        splitted = lines |> splitWhen (== "")
        seeds = splitted |> head |> head
            |> pack |> splitOn ":" |> tail |> head
            |> splitOn " " |> filter (/= "") |> map unpack |> toInt
            |> group 2 |> map (\(x:y:_) -> (False, (x, y)))

        mapping = splitted |> tail |> map tail |> map parseMapRangeL

        func sds invMs = concatMap (`rangeFolder` invMs) sds

main :: IO ()
main = do
    file <- readFile "day_05/input.txt"
    let splLines = lines file
    putStr "Part 1: "
    print $ parse splLines
    putStr "Part 2: "
    print $ parse' splLines
    