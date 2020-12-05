import Data.List

middle :: Int -> Int -> Int
middle from to = (to - from) `div` 2 + from


chooseHalf :: (Int, Int) -> Char -> (Int, Int)
chooseHalf (from, to) c
  | c == 'B' || c == 'R' = (middle from to + 1, to)
  | c == 'F' || c == 'L' = (from, middle from to)


seatId :: String -> Int
seatId input = row * 8 + col
  where (row, _)     = foldl chooseHalf (0, 127) rows
        (col, _)     = foldl chooseHalf (0, 7)   cols
        (rows, cols) = splitAt 7 input


maxId :: [String] -> Int
maxId = maximum . map seatId


mySeat :: [String] -> Int
mySeat input = fst withGap + 1
  where sortedSeats = sort $ map seatId input
        pairs       = zip sortedSeats (tail sortedSeats)
        withGap     = head $ filter (\(x, y) -> y - x > 1) pairs


main = do
  input <- fmap lines $ readFile "input.txt"
  print $ maxId input
  print $ mySeat input