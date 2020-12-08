import qualified Data.Map as Map
import qualified Data.Set as Set


data Instruction = ACC Int | JMP Int | NOP deriving Show
type Program     = Map.Map Int Instruction
type Memory      = Set.Set Int


makeOp :: String -> Int -> Instruction
makeOp "acc" n = ACC n
makeOp "jmp" n = JMP n
makeOp "nop" _ = NOP


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


runLoop :: Program -> Memory -> Int -> Int -> Int
runLoop p mem acc ip 
  | Set.member ip mem = acc
  | otherwise         = case Map.lookup ip p of
                          Nothing      -> acc
                          Just (ACC n) -> runLoop p newMem (acc + n) (ip + 1)
                          Just (JMP n) -> runLoop p newMem acc (ip + n)
                          Just NOP     -> runLoop p newMem acc (ip + 1)
    where newMem = Set.insert ip mem


run :: Program -> Int
run p = runLoop p Set.empty 0 0


main = do
  f <- readFile "input.txt"
  print $ run $ compile f