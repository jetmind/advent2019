import Data.List.Split
import Data.List.Utils
import qualified Data.Set as Set


yesAnyone = sum . map (length . Set.fromList) . map (replace "\n" "") . splitOn "\n\n"


yesEveryone input = sum $ map length common
  where groups = splitOn "\n\n" input
        sets   = map (map Set.fromList . filter (not . null) . splitOn "\n") groups
        common = map (\(x:xs) -> foldr Set.intersection x xs) sets


main = do
  f <- readFile "input.txt"
  print $ yesAnyone f
  print $ yesEveryone f