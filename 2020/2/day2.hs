import Text.Regex.Posix


type Pattern = (Char, Int, Int)
type Password = String


parse :: String -> (Pattern, Password)
parse line = (pat, pwd)
  where pat = (head c, read from, read to)
        [[_, from, to, c, pwd]] = line =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: [[String]]


isInvalid :: (Pattern, Password) -> Bool
isInvalid ((c, from, to), pwd) = count >= from && count <= to
  where count = length $ filter (\x -> x == c) pwd


isValid :: (Pattern, Password) -> Bool
isValid ((c, pos1, pos2), pwd) = length filtered == 1
  where filtered = filter (\(i, x) -> x == c && i `elem` [pos1, pos2]) indexed
        indexed = zip [1..] pwd


main = do
  f <- readFile "input.txt"
  let solve1 = length . filter isInvalid . map parse . lines
  let solve2 = length . filter isValid . map parse . lines
  print $ solve1 f
  print $ solve2 f