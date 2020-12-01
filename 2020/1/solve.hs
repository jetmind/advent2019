parse :: [String] -> [Integer]
parse = map read

solve1 :: [Integer] -> Integer
solve1 l = head [x * y | x <- l, y <- l, x > y, x + y == 2020]

solve2 :: [Integer] -> Integer
solve2 l = head [x * y * z | x <- l, y <- l, z <- l, x > y, y > z, x + y + z == 2020]

main = do
  f <- readFile "input.txt"
  let parsed = (parse . lines) f
  print $ solve1 parsed
  print $ solve2 parsed