import Data.List (partition, sortBy)

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) inp fun = fun inp

readInt :: String -> Int
readInt = read

data Card =
    A | K | Q | J | T
    | Nine | Eight | Seven | Six | Five | Four
    | Three | Two
    deriving (Show, Eq, Ord)

readCard 'A' = A
readCard 'K' = K
readCard 'Q' = Q
readCard 'J' = J
readCard 'T' = T
readCard '9' = Nine
readCard '8' = Eight
readCard '7' = Seven
readCard '6' = Six
readCard '5' = Five
readCard '4' = Four
readCard '3' = Three
readCard '2' = Two

data HandType = FiveKind | FourKind | FullHouse
    | ThreeKind | TwoPair | OnePair | HighCard
    deriving (Show, Eq, Ord)

data Hand = Hand {
    hand :: [Card],
    htype :: HandType,
    bet :: Int
}
    deriving (Show)

groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go [] where
  go acc comp [] = acc
  go acc comp (h:t) =
    let (hs, nohs) = partition (comp h) t
    in go ((h:hs):acc) comp nohs

group = groupBy2 (==)

isFiveKind :: [Card] -> Bool
isFiveKind xs = (group xs |> length) == 1

isFourKind :: [Card] -> Bool
isFourKind xs
    | length grouped /= 2 = False
    | otherwise = (grouped !! 0 |> length) == 4 || (grouped !! 1 |> length) == 4
    where
        grouped = group xs

isFullHouse :: [Card] -> Bool
isFullHouse xs
    | length grouped /= 2 = False
    | otherwise =
        (grouped !! 0 |> length) == 3 && (grouped !! 1 |> length) == 2
        || (grouped !! 0 |> length) == 2 && (grouped !! 1 |> length) == 3
    where
        grouped = group xs

isThreeKind :: [Card] -> Bool
isThreeKind xs = (grouped !! 0) |> length == 3
    where
        grouped = group xs |> sortBy (\a b -> compare (length b) (length a))

isTwoPair :: [Card] -> Bool
isTwoPair xs
    | length grouped /= 3 = False
    | length (grouped !! 0) == 2 && length (grouped !! 1) == 2 = True
    | otherwise = False
    where
        grouped = group xs |> sortBy (\a b -> compare (length b) (length a))

isOnePair :: [Card] -> Bool
isOnePair xs = if grouped !! 0 |> length == 2 then True else False
    where
        grouped = group xs |> sortBy (\a b -> compare (length b) (length a))

verifyHandType :: [Card] -> HandType
verifyHandType xs
    | isFiveKind xs = FiveKind
    | isFourKind xs = FourKind
    | isFullHouse xs = FullHouse
    | isThreeKind xs = ThreeKind
    | isTwoPair xs = TwoPair
    | isOnePair xs = OnePair
    | otherwise = HighCard

parseHand :: String -> [Card]
parseHand "" = []
parseHand (x:xs) = readCard x : parseHand xs

parseLine :: String -> Hand
parseLine line = Hand {hand = cards, htype = handType, bet = handBet}
    where
        splitted = words line
        cards = head splitted |> parseHand
        handType = verifyHandType cards
        handBet = readInt (splitted !! 1)

compareCards :: [Card] -> [Card] -> Ordering
compareCards (x:xs) (y:ys)
    | verify == LT || verify == GT = verify
    | otherwise = compareCards xs ys
    where
        verify = compare x y

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2
    | verifyType == LT || verifyType == GT = verifyType
    | otherwise = compareCards (hand h1) (hand h2)
    where
        verifyType = compare (htype h1) (htype h2)

parse file =
    file
    |> map parseLine
    |> sortBy compareHands
    |> map bet
    |> reverse
    |> zip [1..]
    |> map (\(a, b) -> a * b)
    |> sum

main :: IO ()
main = do
    file <- readFile "day_07/input.txt"
    let splLines = lines file
    putStr "Part 1: "
    print $ parse splLines