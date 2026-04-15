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

signedClicks :: Direction -> Integer -> Integer
signedClicks LeftRotation = negate
signedClicks RightRotation = id

applyClicksInDirectionFromTo :: Instruction -> Integer -> Integer
applyClicksInDirectionFromTo (Instruction dir clicks) start = (start + signedClicks dir clicks) `mod` dialSize

applyClicks :: Integer -> String -> Integer
applyClicks acc cur = applyClicksInDirectionFromTo (parseInstruction cur) acc

applyScan :: String -> [Integer]
applyScan = scanl applyClicks beginningLocation . lines

countZeroes :: (Eq a, Num a) => [a] -> Int
countZeroes = length . filter (== 0)

process :: String -> IO ()
process = print . countZeroes . applyScan

main = do
  s <- readFile "inputs/1.txt"
  process s
