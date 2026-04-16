data Direction = LeftRotation | RightRotation

data Instruction = Instruction Direction Integer

beginningLocation :: Integer
beginningLocation = 50

dialSize :: Integer
dialSize = 100

charToDirection :: Char -> Direction
charToDirection 'L' = LeftRotation
charToDirection 'R' = RightRotation

parseInstruction :: String -> Instruction
parseInstruction (c:rest) = Instruction (charToDirection c) $ read rest

parseInstructions :: String -> [Instruction]
parseInstructions = map parseInstruction . lines

signedClicks :: Direction -> Integer -> Integer
signedClicks LeftRotation = negate
signedClicks RightRotation = id

applyInstructionFrom :: Integer -> Instruction -> Integer
applyInstructionFrom start (Instruction dir clicks) = (start + signedClicks dir clicks) `mod` dialSize

startingLocations :: [Instruction] -> [Integer]
startingLocations = scanl applyInstructionFrom beginningLocation

locationsAfterEachInstruction :: [Instruction] -> [Integer]
locationsAfterEachInstruction instructions =
  zipWith applyInstructionFrom (startingLocations instructions) instructions

locationAfterClick :: Direction -> Integer -> Integer -> Integer
locationAfterClick dir start click = applyInstructionFrom start (Instruction dir click)

applyClicksInDirectionFromThrough :: Instruction -> Integer -> [Integer]
applyClicksInDirectionFromThrough (Instruction dir clicks) start =
  map (locationAfterClick dir start) [1..clicks]

locationsThroughEachInstruction :: [Instruction] -> [[Integer]]
locationsThroughEachInstruction instructions =
  zipWith applyClicksInDirectionFromThrough instructions starts
  where
    starts = startingLocations instructions

locationsThroughAllInstructions :: [Instruction] -> [Integer]
locationsThroughAllInstructions = concat . locationsThroughEachInstruction

countZeroes :: (Eq a, Num a) => [a] -> Int
countZeroes = length . filter (== 0)

countPartOneZeroes :: [Instruction] -> Int
countPartOneZeroes = countZeroes . locationsAfterEachInstruction

countPartTwoZeroes :: [Instruction] -> Int
countPartTwoZeroes = countZeroes . locationsThroughAllInstructions

process :: String -> IO ()
process input = do
  let instructions = parseInstructions input
  putStrLn $ "Part 1: " ++ show (countPartOneZeroes instructions)
  putStrLn $ "Part 2: " ++ show (countPartTwoZeroes instructions)

main = do
  s <- readFile "inputs/1.txt"
  process s
