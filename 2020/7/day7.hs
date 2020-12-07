import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Regex.PCRE

-- part 1

type Bags          = Set.Set String
type InvertedIndex = Map.Map String Bags


parseLine :: String -> InvertedIndex
parseLine line = Map.fromListWith Set.union [(bag, Set.fromList [outerBag]) | [_, bag] <- innerBags]
  where [[outerBag]] = line =~ "^\\w+ \\w+" :: [[String]]
        innerBags    = line =~ "\\d+ (\\w+ \\w+)" :: [[String]]


parse :: String -> InvertedIndex
parse = foldr (Map.unionWith Set.union) Map.empty . map parseLine . lines


allBags :: Bags -> InvertedIndex -> String -> Bags
allBags acc index bag = case Map.lookup bag index of
  Nothing   -> acc
  Just bags -> Set.foldr Set.union bags $ Set.map (allBags acc index) bags


countBags :: InvertedIndex -> String -> Int
countBags index = length . allBags Set.empty index


-- part 2

type Index = Map.Map String [(Int, String)]


parseLine2 :: String -> (String, [(Int, String)])
parseLine2 line = (outerBag, [(read i, bag) | [_, i, bag] <- innerBags])
  where [[outerBag]] = line =~ "^\\w+ \\w+" :: [[String]]
        innerBags    = line =~ "(\\d+) (\\w+ \\w+)" :: [[String]]


parse2 :: String -> Index
parse2 = Map.fromList . map parseLine2 . lines


countContainingBags :: Int -> Index -> String -> Int
countContainingBags acc index bag = case Map.lookup bag index of
  Nothing   -> acc
  Just bags -> foldr (+) this inner
    where this  = sum $ map fst bags
          inner = map (\(n, bag) -> n * countContainingBags acc index bag) bags


main = do
  f <- readFile "input.txt"
  let invIndex = parse f
  let index    = parse2 f
  print $ countBags invIndex "shiny gold"
  print $ countContainingBags 0 index "shiny gold"