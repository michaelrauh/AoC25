import Data.Char (digitToInt)
import Data.List (tails)
newtype Battery = Battery Int deriving (Eq, Ord)
newtype Bank = Bank [Battery]

unBank :: Bank -> [Battery]
unBank (Bank bs) = bs

findMaxWithRoom :: (Ord a, Eq a) => Int -> [a] -> [a]
findMaxWithRoom 1 l = [maximum l]
findMaxWithRoom room l =
    let largestIntWithRoom = maximum $ take (length l - room + 1) l
        remainingSearchSpace = tail $ dropWhile (/= largestIntWithRoom) l
    in largestIntWithRoom : findMaxWithRoom (room - 1) remainingSearchSpace

parse :: String -> Bank
parse = Bank . map (Battery . digitToInt)

parseAll :: String -> [Bank]
parseAll = map parse . lines

processOne :: [Bank] -> Int
processOne = sum . map findMaxJoltage

findMaxJoltage :: Bank -> Int
findMaxJoltage = listToJoltage . findMaxWithRoom 2 . unBank

listToJoltage :: [Battery] -> Int
listToJoltage = foldl (\acc (Battery d) -> acc * 10 + d) 0

pairToJoltage :: (Battery, Battery) -> Int
pairToJoltage (Battery a, Battery b) = 10 * a + b

process :: String -> IO ()
process input = do
    let banks = parseAll input
    putStrLn $ "Part 1: " ++ show (processOne banks)

main :: IO ()
main = do
  s <- readFile "inputs/3.txt"
  process s
