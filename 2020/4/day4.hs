import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.Posix
import Text.Read


requiredKeys = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


parseOne :: String -> Map.Map String String
parseOne s = Map.fromList [(k, v) | [k, v] <- pairs]
  where pairs = map (splitOn ":") (splitOneOf " \n" s)


parseAll :: String -> [Map.Map String String]
parseAll input = map parseOne $ splitOn "\n\n" input


validKeys :: Map.Map String String -> Bool
validKeys passport = requiredKeys == Set.intersection requiredKeys (Map.keysSet passport)


validYear :: String -> Int -> Int -> Bool
validYear s from to = case readMaybe s :: Maybe Int of
  Just year -> year >= from && year <= to
  Nothing   -> False


validHeight :: String -> Bool
validHeight s = case s =~ "([0-9]+)(cm|in)" :: [[String]] of
  [[_, x, unit]] -> case (readMaybe x :: Maybe Int, unit) of
    (Just x, "cm") -> x >= 150 && x <= 193
    (Just x, "in") -> x >= 59 && x <= 76
    _              -> False
  _ -> False


validValue :: (String, String) -> Bool
validValue ("byr", v) = validYear v 1920 2002
validValue ("iyr", v) = validYear v 2010 2020
validValue ("eyr", v) = validYear v 2020 2030
validValue ("hgt", v) = validHeight v
validValue ("hcl", v) = v =~ "^#[0-9a-f]{6}$"
validValue ("ecl", v) = elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validValue ("pid", v) = v =~ "^[0-9]{9}$"
validValue ("cid", _) = True
validValue (_, _)     = False


validPassport :: Map.Map String String -> Bool
validPassport p = validKeys p && all validValue (Map.toList p)


main = do
  f <- readFile "input.txt"
  let parsed = parseAll f
  print $ length $ filter validKeys parsed
  print $ length $ filter validPassport parsed