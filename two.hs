import Data.Monoid (Sum(..))
import Data.List.Split (splitOn)

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

isInvalid :: ID -> Bool
isInvalid (ID n) = uncurry (==) $ splitAt (length s `div` 2) s
  where
    s = show n

findInvalidIds :: [Range] -> [ID]
findInvalidIds = filter isInvalid . findAllIds

process :: String -> IO ()
process input = do
  putStrLn $ "Part 1: " ++ show (processOne input)

processOne :: String -> Integer
processOne = sumIDs . findInvalidIds . parseAll

main = do
  s <- readFile "inputs/2.txt"
  process s
