import Control.Monad.State

(.:) = (.) . (.)

type FibState = State (Int, Int)

initialFib :: (Int, Int)
initialFib = (1, 0)

computeNextFib :: FibState ()
computeNextFib = modify $ \(a, b) -> (a + b, a)

withFib :: State (Int, Int) a -> a
withFib = (`evalState` initialFib)

currFib :: FibState Int
currFib = do
  (f, _) <- get
  return f

nextFib :: FibState Int
nextFib = do
  computeNextFib
  currFib

runN :: Monad m => Int -> m a -> m ()
runN n t
  | n > 0 = do
    t
    runN (n - 1) t
runN 0 t = return ()

genN :: Monad m => Int -> m a -> m [a]
genN n t
  | n > 0 = do
    x <- t
    xs <- genN (n - 1) t
    return $ x : xs
genN 0 t = return []

print3 :: IO ()
print3 =
  withFib $ do
    f1 <- nextFib
    f2 <- nextFib
    f3 <- nextFib
    return $ forM_ [f1, f2, f3] print

print3' :: IO ()
print3' =
  withFib $ do
    fibs <- genN 3 nextFib
    return $ forM_ fibs print

nFibs :: Int -> [Int]
nFibs n = withFib $ genN n nextFib

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)

fork :: State x a -> (a -> b) -> State x b
-- fork st f = state $ \s -> (f $ evalState st s, s)
-- fork s f = state $ toFst (f . evalState s)
fork s = state . toFst . flip (.) (evalState s)

fork2 :: State x a -> State x b -> (a -> b -> c) -> State x c
-- fork2 s1 s2 join = state $ \s -> (evalState s1 s `join` evalState s2 s, s)
-- fork2 s1 s2 join = state $ toFst (\s -> evalState s1 s `join` evalState s2 s)
fork2 s1 s2 join = do
  a <- fork s1 id
  b <- fork s2 id
  return $ join a b

splitted =
  withFib $ do
    genN 5 nextFib
    fork2 nextFib currFib $ \a b -> do
      print a
      print b
