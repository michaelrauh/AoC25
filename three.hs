processOne :: String
processOne = "hello"

process :: String -> IO ()
process input = do
  putStrLn $ "Part 1: " ++ show processOne

main = do
  s <- readFile "inputs/3.txt"
  process s
