import Data.List (transpose)

data Problem = Problem [Int] Op
data Op = Add | Multiply

parseOp :: String -> Op
parseOp "+" = Add
parseOp _   = Multiply

parseAll :: String -> [Problem]
parseAll input =
    let ls       = filter (not . null . words) (lines input)
        dataRows = init ls
        opRow    = last ls
        numGrid  = map (map read . words) dataRows
        opList   = map parseOp (words opRow)
        cols     = transpose numGrid
    in zipWith Problem cols opList

parseAll2 :: String -> [Problem]
parseAll2 input =
    let ls       = lines input
        maxLen   = maximum (map length ls)
        padded   = map (\l -> l ++ replicate (maxLen - length l) ' ') ls
        dataRows = init padded
        opRow    = last padded
        dataCols = transpose dataRows
        paired   = zip dataCols opRow
        groups   = splitGroups paired
    in map groupToProblem groups

splitGroups :: [(String, Char)] -> [[(String, Char)]]
splitGroups [] = []
splitGroups xs =
    let rest1 = dropWhile isSep xs
    in case rest1 of
        [] -> []
        _  -> let (group, rest2) = break isSep rest1
              in group : splitGroups rest2
  where isSep (col, op) = all (== ' ') col && op == ' '

groupToProblem :: [(String, Char)] -> Problem
groupToProblem pairs =
    let opChar = head [c | (_, c) <- pairs, c /= ' ']
        op     = parseOp [opChar]
        nums   = map (read . filter (/= ' ') . fst) pairs
    in Problem nums op

solve :: Problem -> Int
solve (Problem nums Add)      = sum nums
solve (Problem nums Multiply) = product nums

total :: [Problem] -> Int
total = sum . map solve

process :: String -> IO ()
process input = do
  let problems = parseAll input
  putStrLn $ "Part 1: " ++ show (total problems)
  let problems2 = parseAll2 input
  putStrLn $ "Part 2: " ++ show (total problems2)

main :: IO ()
main = do
  s <- readFile "inputs/6.txt"
  process s