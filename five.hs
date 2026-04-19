data Database = Database BrokenRange [ID]
newtype BrokenRange = BrokenRange [(ID, ID)]
newtype ID = ID Int deriving (Eq, Ord)

insertRange :: ID -> ID -> [(ID, ID)] -> [(ID, ID)]
insertRange s e [] = [(s, e)]
insertRange s e ((lo, hi) : rest)
  | e < lo    = (s, e) : (lo, hi) : rest
  | s > hi    = (lo, hi) : insertRange s e rest
  | otherwise = insertRange (min s lo) (max e hi) rest



addRange :: ID -> ID -> BrokenRange -> BrokenRange
addRange start end (BrokenRange ranges) = BrokenRange (insertRange start end ranges)

countAllIDs :: BrokenRange -> Int
countAllIDs (BrokenRange ranges) = sum (map (\(ID lo, ID hi) -> hi - lo + 1) ranges)

inRange :: BrokenRange -> ID -> Bool
inRange (BrokenRange []) _ = False
inRange (BrokenRange ((lo, hi) : rest)) x
  | x < lo    = False
  | x <= hi   = True
  | otherwise = inRange (BrokenRange rest) x

emptyBrokenRange :: BrokenRange
emptyBrokenRange = BrokenRange []

findBreak :: String -> ([String], [String])
findBreak input =
  let (firstHalf, rest) = break null (lines input)
  in (firstHalf, drop 1 rest)

parseRange :: String -> (ID, ID)
parseRange line =
  let (startStr, endWithDash) = break (== '-') line
  in (ID (read startStr), ID (read (drop 1 endWithDash)))

addParsedRange :: BrokenRange -> String -> BrokenRange
addParsedRange acc line =
  let (startId, endId) = parseRange line
  in addRange startId endId acc

parseFirstHalf :: [String] -> BrokenRange
parseFirstHalf = foldl addParsedRange emptyBrokenRange

parseSecondHalf :: [String] -> [ID]
parseSecondHalf = map (ID . read)

parseAll :: String -> Database
parseAll input =
  let (firstHalf, secondHalf) = findBreak input
      ranges = parseFirstHalf firstHalf
      ids = parseSecondHalf secondHalf
  in Database ranges ids

process :: String -> IO ()
process input = do
  let Database range ids = parseAll input
      freshCount = length (filter (inRange range) ids)
  putStrLn $ "Part 1: " ++ show freshCount
  putStrLn $ "Part 2: " ++ show (countAllIDs range)

main :: IO ()
main = do
  s <- readFile "inputs/5.txt"
  process s