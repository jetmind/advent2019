import Data.List


count input = ones * (threes + 1)
  where diffs = zipWith (-) input (0:input)
        ones   = length $ filter (==1) diffs
        threes = length $ filter (==3) diffs


main = do
  f <- readFile "input.txt"
  let input = sort $ map read $ lines f :: [Int]
  print $ count input
