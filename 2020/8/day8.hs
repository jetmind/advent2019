import Data.Either
import qualified Data.Map as Map
import qualified Data.Set as Set


data Instruction = ACC Int | JMP Int | NOP Int deriving Show
type Program     = Map.Map Int Instruction
type Memory      = Set.Set Int


makeOp :: String -> Int -> Instruction
makeOp "acc" n = ACC n
makeOp "jmp" n = JMP n
makeOp "nop" n = NOP n


compileInstruction :: String -> Instruction
compileInstruction line = makeOp op (read n)
  where op   = take 3 line
        sign = take 1 $ drop 4 line
        n    = case sign of
                 "-" -> drop 4 line
                 _   -> drop 5 line


compile :: String -> Program
compile input = Map.fromList withIndex
  where withIndex    = zip [0..] instructions
        instructions = map compileInstruction $ lines input


runLoop :: Program -> Memory -> Int -> Int -> Either Int Int
runLoop p mem acc ip 
  | Set.member ip mem = Left acc  -- found a loop
  | otherwise         = case Map.lookup ip p of
                          Nothing      -> Right acc  -- reached the end
                          Just (ACC n) -> runLoop p newMem (acc + n) (ip + 1)
                          Just (JMP n) -> runLoop p newMem acc (ip + n)
                          Just (NOP _) -> runLoop p newMem acc (ip + 1)
    where newMem = Set.insert ip mem


correct :: Program -> [Program] -> Int -> [Program]
correct p ps ip
  | ip >= Map.size p = ps
  | otherwise        = correct p newPs (ip + 1)
    where newPs = case Map.lookup ip p of
                    Just (JMP n) -> (Map.insert ip (NOP n) p) : ps
                    Just (NOP n) -> (Map.insert ip (JMP n) p) : ps
                    _            -> ps


run :: Program -> Either Int Int
run p = runLoop p Set.empty 0 0

runCorrected :: Program -> Int
runCorrected p = head $ rights $ map run programs
  where programs = correct p [] 0


main = do
  f <- readFile "input.txt"
  print $ run $ compile f
  print $ runCorrected $ compile f