import Data.List
import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("Got: " ++ show a) a

data Expr = Start Int Expr | Add Int Expr | Mul Int Expr | Cat Int Expr | End

instance Show Expr where
  show End = ""
  show (Start n e) = show n ++ show e
  show (Add n e) = "+" ++ show n ++ show e
  show (Cat n e) = "||" ++ show n ++ show e
  show (Mul n e) = "*" ++ show n ++ show e

type InputItem = (Int, [Int])

type Input = [InputItem]

expressions :: [Int] -> [Expr]
expressions (i : is) = [Start i e | e <- exprInner is]
  where
    exprInner :: [Int] -> [Expr]
    exprInner [] = [End]
    exprInner (v : vs) = do
      op <- ["Add", "Mul", "Cat"]
      tail <- exprInner vs
      case op of
        "Add" -> return $ Add v tail
        "Mul" -> return $ Mul v tail
        "Cat" -> return $ Cat v tail

eval :: Expr -> Int
eval (Start n e) = evalAcc n e
  where
    evalAcc :: Int -> Expr -> Int
    evalAcc acc End = acc
    evalAcc acc (Cat n e) = evalAcc (cat acc n) e
    evalAcc acc (Add n e) = evalAcc (acc + n) e
    evalAcc acc (Mul n e) = evalAcc (acc * n) e
    cat x y = read $ show x ++ show y

isPossible :: InputItem -> Bool
isPossible = maybe False (const True) . findPossible

findPossible :: InputItem -> Maybe Expr
findPossible (t, xs) = find (\n -> t == eval n) $ expressions xs

inputFromString :: String -> Input
inputFromString contents = map item $ lines contents
  where
    item :: String -> InputItem
    item line = let w : ws = words line in (read $ trim w, map read ws)
    trim :: String -> String
    trim (':' : ss) = ""
    trim (s : ss) = s : trim ss

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  let input = inputFromString contents
  let possible = filter isPossible input
  let total = sum $ map fst possible
  putStrLn $ show (total, length possible)
