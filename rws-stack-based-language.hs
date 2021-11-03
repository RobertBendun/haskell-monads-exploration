import Control.Monad
import Control.Monad.Trans.RWS.Lazy
import Data.Char

data Operation
  = Push Int
  | Add
  | Print
  | Dup
  | Halt
  | Subtract
  | JumpIfNotZero
  deriving (Show, Eq)

type Stdout = [String]

type IP = Int -- Instruction Pointer

type Stack = [Int]

type Bytecode = [Int]

-- TODO Machine state should be instruction pointer AND memory
type MachineOp = RWS Bytecode Stdout IP

type Machine = MachineOp (Either String Stack)

decodeInstruction :: MachineOp (Either String Operation)
decodeInstruction = do
  ip <- get
  bytecode <- ask
  case drop ip bytecode of
    (0:x:_) -> decoded 2 $ Push x
    (1:_) -> decoded 1 Add
    (2:_) -> decoded 1 Print
    (3:_) -> decoded 1 Dup
    (4:_) -> decoded 1 Subtract
    (5:_) -> decoded 1 JumpIfNotZero
    [] -> decoded 1 Halt
    xs -> return $ Left $ "Unknown bytecode: " ++ show xs
  where
    decoded n v = do
      ip <- get
      put $ ip + n
      return $ Right v

instr :: Stack -> Operation -> Machine
instr stack Halt = return $ Right stack
instr stack (Push x) = execute $ x : stack
instr (a:b:rest) Add = execute $ (a + b) : rest
instr _ Add = return $ Left "Not enought data for Add operation"
instr (a:b:rest) Subtract = execute $ (b - a) : rest
instr _ Subtract = return $ Left "Not enought data for Subtract operation"
instr (a:rest) Dup = execute $ a : a : rest
instr _ Dup = return $ Left "Not enought data for Dup operation"
instr (a:rest) Print = do
  tell [show a]
  execute rest
instr _ Print = return $ Left "Not enough data for Print operation"
instr (target:value:rest) JumpIfNotZero =
  if value /= 0
    then do
      put target
      execute rest
    else execute rest
instr _ JumpIfNotZero =
  return $ Left "Not enough data for Jump If Not Zero operation"

execute :: Stack -> Machine
execute stack = do
  op <- decodeInstruction
  case op of
    (Right op) -> instr stack op
    (Left err) -> return $ Left err

runMachine :: Bytecode -> (Either String Stack, Stdout)
runMachine memoryMap = evalRWS (execute []) memoryMap ip
  where
    ip = 0

run :: Bytecode -> IO ()
run = summary . runMachine
  where
    summary :: (Either String Stack, Stdout) -> IO ()
    summary (Left message, _) = do
      putStr "Error: "
      print message
    summary (Right stack, stdout) = do
      putStrLn "Stdout: "
      forM_ stdout (putStrLn . ("  " ++))
      unless (null stack) $ do
        putStr "Stack: "
        print stack

compile :: String -> Bytecode
compile = c . words
  where
    c :: [String] -> [Int]
    c (n:rest)
      | all isDigit n = [0, read n] ++ c rest
    c ("+":rest) = 1 : c rest
    c (".":rest) = 2 : c rest
    c ("dup":rest) = 3 : c rest
    c ("-":rest) = 4 : c rest
    c ("jnz":rest) = 5 : c rest
    c [] = []
    c x = error $ "Invalid command: " ++ show x

basic = "10 305 dup . + ."

loop = "10 dup . 1 - dup 2 jnz ."

main :: IO ()
main =
  forM_ examples $ \example -> do
    divider
    putStrLn example
    divider
    run $ compile $ example
  where
    examples = [basic, loop]
    dividerLength = maximum $ map length examples
    divider = putStrLn $ replicate dividerLength '-'
