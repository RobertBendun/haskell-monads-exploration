import Control.Monad
import Control.Monad.Trans.RWS.Lazy
import Data.Char

-- 😂 Concatenative Functional Languages >>> Haskell
-- https://evincarofautumn.blogspot.com/2012/02/why-concatenative-programming-matters.html
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

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

keywords = zip ["+", ".", "dup", "-", "jnz"] [1 ..]

decodeOperation :: MachineOp (Either String Operation)
decodeOperation = do
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

-- Math related binary operation
mbin :: Stack -> Operation -> (Int -> Int -> Int) -> Machine
mbin (a:b:rest) _ f = execute $ f b a : rest
mbin _ op _ = return $ Left $ "Not enough data for " ++ show op ++ " operation"

instr :: Stack -> Operation -> Machine
instr stack Halt = return $ Right stack
instr stack (Push x) = execute $ x : stack
instr stack Add = mbin stack Add (+)
instr stack Subtract = mbin stack Subtract (-)
instr (a:rest) Dup = execute $ a : a : rest
instr _ Dup = return $ Left "Not enought data for Dup operation"
instr (a:rest) Print = do
  tell [show a]
  execute rest
instr _ Print = return $ Left "Not enough data for Print operation"
instr (target:value:rest) JumpIfNotZero = do
  when (value /= 0) $ put target
  execute rest
instr _ JumpIfNotZero =
  return $ Left "Not enough data for Jump If Not Zero operation"

execute :: Stack -> Machine
execute stack = do
  op <- decodeOperation
  case op of
    (Right op) -> instr stack op
    (Left err) -> return $ Left err

runMachine :: Stack -> Bytecode -> (Either String Stack, Stdout)
runMachine stack bytecode = evalRWS (execute stack) bytecode ip
  where
    ip = 0

run :: Stack -> Bytecode -> IO Stack
run = summary .: runMachine
  where
    summary :: (Either String Stack, Stdout) -> IO Stack
    summary (Left message, _) = do
      putStr "Execution error: "
      print message
      return []
    summary (Right stack, stdout) = do
      forM_ stdout (putStrLn . ("  " ++))
      unless (null stack) $ do
        putStr "Stack: "
        print stack
      return stack

compile :: String -> Either String Bytecode
compile = c . words
  where
    c :: [String] -> Either String Bytecode
    c (n:rest)
      | all isDigit n = ([0, read n] ++) <$> c rest
    c (k:rest) =
      case [id | (keyword, id) <- keywords, keyword == k] of
        [v] -> (v :) <$> c rest
        _ -> Left $ "Invalid command: " ++ k
    c [] = Right []

compileAndRun :: Stack -> String -> IO Stack
compileAndRun stack program =
  case compile program of
    Right bytecode -> run stack bytecode
    Left err -> do
      putStr "Compilation error: "
      putStrLn err
      return stack

repl :: IO ()
repl = loop []
  where
    loop :: Stack -> IO ()
    loop stack = do
      putStr "> "
      program <- getLine
      case program of
        "exit" -> return ()
        "bye" -> return ()
        _ -> do
          stack' <- compileAndRun stack program
          loop stack'

basic = "10 305 dup . + ."

loop = "10 dup . 1 - dup 2 jnz ."

main :: IO ()
main =
  forM_ examples $ \example -> do
    divider
    putStrLn example
    divider
    compileAndRun [] example
  where
    examples = [basic, loop]
    dividerLength = maximum $ map length examples
    divider = putStrLn $ replicate dividerLength '-'
