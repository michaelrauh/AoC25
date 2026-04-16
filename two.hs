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

sumIDs :: [ID] -> Integer
sumIDs = getSum . foldMap toSum
  where
    toSum (ID n) = Sum n

divisibleLengths :: String -> [Integer]
divisibleLengths n = [d | d <- [1..(len `div` 2)], len `mod` d == 0]
  where
    len = fromIntegral $ length n

isRepeating :: String -> Integer -> Bool
isRepeating s len = all (== take (fromIntegral len) s) (chunksOf (fromIntegral len) s)

hasRepeatingCycle :: String -> [Integer] -> Bool
hasRepeatingCycle s = any (isRepeating s)

isInvalidWith :: (String -> [Integer]) -> ID -> Bool
isInvalidWith cycleLengths (ID n) = hasRepeatingCycle s (cycleLengths s)
  where
    s = show n

partOneCycleLengths :: String -> [Integer]
partOneCycleLengths s
  | even len = [fromIntegral (len `div` 2)]
  | otherwise = []
  where
    len = length s

isInvalidPartOne :: ID -> Bool
isInvalidPartOne = isInvalidWith partOneCycleLengths

isInvalidPartTwo :: ID -> Bool
isInvalidPartTwo = isInvalidWith divisibleLengths

findInvalidIds :: (ID -> Bool) -> [Range] -> [ID]
findInvalidIds isInvalid = filter isInvalid . findAllIds

processOne :: [Range] -> Integer
processOne = sumIDs . findInvalidIds isInvalidPartOne

processTwo :: [Range] -> Integer
processTwo = sumIDs . findInvalidIds isInvalidPartTwo

process :: String -> IO ()
process input = do
  let ranges = parseAll input
  putStrLn $ "Part 1: " ++ show (processOne ranges)
  putStrLn $ "Part 2: " ++ show (processTwo ranges)

main = do
  s <- readFile "inputs/2.txt"
  process s
