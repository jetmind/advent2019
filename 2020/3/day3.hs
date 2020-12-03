import qualified Data.Set as Set


type Coord = (Int, Int)
type Path = [Coord]
data Map = Map { width :: Int,
                 height :: Int,
                 trees :: Set.Set Coord }
         deriving Show


parse :: [String] -> Map
parse input = Map { width = w, height = h, trees = t }
  where w = length $ head input
        h = length input
        t = Set.fromList [(x, y) | (y, row) <- zip [0..] input,
                                   (x, dot) <- zip [0..] row,
                                   dot == '#']

path :: Map -> (Int, Int) -> Path
path m (stepX, stepY) = [(i * stepX, i * stepY) | i <- [0 .. (height m)]]


isTree :: Map -> Coord -> Bool
isTree Map{ width=w, trees=t } (x, y) =
  Set.member (x `mod` w, y) t


countTrees :: Map -> Path -> Int
countTrees m p = length $ filter (isTree m) p


solve1 :: Map -> Int
solve1 m = countTrees m (path m (3, 1))


solve2 :: Map -> Int
solve2 m = product $ map (countTrees m) paths
  where paths = map (path m) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]


main = do
  f <- readFile "input.txt"
  let m = (parse . lines) f
  print $ solve1 m
  print $ solve2 m
