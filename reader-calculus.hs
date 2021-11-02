
import Control.Monad.Reader

data Term = Apply Term Term | Lambda String Term | Var Term
  deriving (Show)

newtype Env = Env [(String, Closure)]
type Closure = (Term, Env)

data Value = Lam String Closure | Failure String

eval :: Term -> Reader Env Value
eval (Lambda nv t) = do
  env <- ask
  return $ Lam nv (t, env)

eval (Var v) = do
  (Env env) <- ask
  case lookup (show v) env of
    Nothing -> return . Failure $ "unbounded variable: " ++ show v
    Just (term, env) -> local (const env) $ eval term
  
eval (Apply t1 t2) = do
  v1 <- eval t1
  case v1 of
    Failure s -> return $ Failure s
    Lam nv clos -> local (\(Env ls) -> Env ((nv, clos) : ls)) $ eval t2

evaluate :: Term -> Value
evaluate term = runReader (eval term) (Env [])