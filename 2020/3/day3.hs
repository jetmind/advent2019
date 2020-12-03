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

path :: Map -> Path
path m = [(i * 3, i) | i <- [0 .. (height m)]]


isTree :: Map -> Coord -> Bool
isTree Map{ width=w, trees=t } (x, y) =
  Set.member (x `mod` w, y) t


countTrees :: Map -> Path -> Int
countTrees m p = length $ filter (isTree m) p


main = do
  f <- readFile "input.txt"
  let m = (parse . lines) f
      p = path m
  print $ countTrees m p
