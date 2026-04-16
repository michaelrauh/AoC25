import Data.Monoid (Sum(..))
import Data.List.Split (splitOn, chunksOf)

data Range = Range Integer Integer
newtype ID = ID Integer

parse :: String -> Range
parse s = Range start end
  where
    [start, end] = map read $ splitOn "-" s

parseAll :: String -> [Range]
parseAll = map parse . splitOn ","

findIDsInRange :: Range -> [ID]
findIDsInRange (Range start end) = map ID [start .. end]

findAllIds :: [Range] -> [ID]
findAllIds = concatMap findIDsInRange

idToSum :: ID -> Sum Integer
idToSum (ID n) = Sum n

sumIDs :: [ID] -> Integer
sumIDs = getSum . foldMap idToSum

divisibleLengths :: String -> [Integer]
divisibleLengths n = [d | d <- [1..(len `div` 2)], len `mod` d == 0]
  where
    len = fromIntegral $ length n

isRepeating :: String -> Integer -> Bool
isRepeating s len = all (== take (fromIntegral len) s) (chunksOf (fromIntegral len) s)

hasRepeatingCycle :: String -> [Integer] -> Bool
hasRepeatingCycle s = any (isRepeating s)

isInvalidPartOne :: ID -> Bool
isInvalidPartOne (ID n) =
  even len && hasRepeatingCycle s [fromIntegral halfLen]
  where
    s = show n
    len = length s
    halfLen = len `div` 2

isInvalidPartTwo :: ID -> Bool
isInvalidPartTwo (ID n) = hasRepeatingCycle s (divisibleLengths s)
  where
    s = show n

findInvalidIdsPartOne :: [Range] -> [ID]
findInvalidIdsPartOne = filter isInvalidPartOne . findAllIds

findInvalidIdsPartTwo :: [Range] -> [ID]
findInvalidIdsPartTwo = filter isInvalidPartTwo . findAllIds

process :: String -> IO ()
process input = do
  putStrLn $ "Part 1: " ++ show (processOne input)
  putStrLn $ "Part 2: " ++ show (processTwo input)

processOne :: String -> Integer
processOne = sumIDs . findInvalidIdsPartOne . parseAll

processTwo :: String -> Integer
processTwo = sumIDs . findInvalidIdsPartTwo . parseAll

main = do
  s <- readFile "inputs/2.txt"
  process s
