import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Place = Roll | Floor deriving (Show, Eq)
type Pos = (Int, Int)
type Grid = Map Pos Place
type CountGrid = Map Pos Int

parseChar :: Char -> Place
parseChar '@' = Roll
parseChar _   = Floor

parseGrid :: String -> Grid
parseGrid input = Map.fromList
  [ ((r, c), parseChar ch)
  | (r, row) <- zip [0..] (lines input)
  , (c, ch)  <- zip [0..] row
  ]

neighbors :: Pos -> [Pos]
neighbors (r, c) = [(r+dr, c+dc) | dr <- [-1,0,1], dc <- [-1,0,1], (dr,dc) /= (0,0)]

adjacentPlaces :: Grid -> Pos -> [Place]
adjacentPlaces grid pos = Map.elems (Map.restrictKeys grid (Set.fromList (neighbors pos)))

countAt :: Grid -> Pos -> Place -> Int
countAt _ _ Floor = 0
countAt grid pos Roll  = length (filter (== Roll) (adjacentPlaces grid pos))

countAdjacent :: Grid -> CountGrid
countAdjacent grid = Map.mapWithKey (countAt grid) (Map.filter (== Roll) grid)

filterGrid :: CountGrid -> CountGrid
filterGrid = Map.filter (< 4)

gridSum :: CountGrid -> Int
gridSum = Map.size

step :: Grid -> (Grid, Int)
step grid =
  let removable = filterGrid (countAdjacent grid)
      removed = Map.size removable
      grid' = Map.withoutKeys grid (Map.keysSet removable)
  in (grid', removed)

solve :: Grid -> Int
solve grid =
  let (grid', removed) = step grid
  in if removed == 0 then 0 else removed + solve grid'

process :: String -> IO ()
process input = do
  let grid = parseGrid input
      counts = countAdjacent grid
  putStrLn $ "Part 1: " ++ show (gridSum (filterGrid counts))
  putStrLn $ "Part 2: " ++ show (solve grid)

main :: IO ()
main = do
  s <- readFile "inputs/4.txt"
  process s