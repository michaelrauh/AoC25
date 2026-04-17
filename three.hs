{-# LANGUAGE TupleSections #-}
import Data.Char (digitToInt)
import Data.List (tails)
newtype Battery = Battery Int
newtype Bank = Bank [Battery]

unBank :: Bank -> [Battery]
unBank (Bank bs) = bs


-- example input
-- 987654321111111
-- 811111111111119
-- 234234234234278
-- 818181911112111

parse :: String -> Bank
parse = Bank . map (Battery . digitToInt)

parseAll :: String -> [Bank]
parseAll = map parse . lines

processOne :: [Bank] -> Int
processOne = sum . map findMaxJoltage

pairs :: [a] -> [(a, a)]
pairs =
  concatMap (\(x:xs) -> map (x,) xs)
  . filter (not . null)
  . tails

findMaxJoltage :: Bank -> Int
findMaxJoltage = maximum . map pairToJoltage . pairs . unBank

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
