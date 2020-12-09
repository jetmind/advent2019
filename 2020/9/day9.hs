import Data.List


valid :: [Int] -> [Int]
valid window = [a + b | a <- window, b <- window, a > b]


findInvalid :: [Int] -> Int
findInvalid numbers = fst $ head invalid
  where windows = map (take 25) $ tails numbers
        nums    = zip (drop 25 numbers) windows
        invalid = filter (\(n, w) -> n `notElem` (valid w)) nums


weakness :: Int -> [Int] -> Int
weakness n nums = minimum chunk + maximum chunk
  where chunk = head $ filter (\xs -> sum xs == n) $ concatMap inits $ tails nums


main = do
  f <- readFile "input.txt"
  let nums    = map read $ lines f :: [Int]
      invalid = findInvalid nums
  print invalid
  print $ weakness invalid nums