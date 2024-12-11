import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Char
import Data.Function
import Data.IntMap.Lazy qualified as M

input = "" -- input here

initialStones :: [Int]
initialStones = map read $ words input

evenDigits :: Int -> Bool
evenDigits = (== 0) . (`mod` 2) . length . show

split :: Int -> (Int, Int)
split i = (read l, read r)
  where
    str = show i
    len = length str
    (l, r) = splitAt (len `div` 2) str

type Cache = M.IntMap [Int]

type S = State Cache [Int]

data Rule = Zero | Split | Mul deriving (Show)

rule :: Int -> Rule
rule i
  | evenDigits i = Split
  | i == 0 = Zero
  | otherwise = Mul

-- put result of ct in cache -before- ct is run with said cache
cacheFirst :: Int -> S -> S
cacheFirst d ct = StateT $ \c ->
  let update seq = M.insert d seq c
   in Identity . fix $ (runState ct) . update . fst

blinking :: Int -> S
blinking d = do
  cache <- get
  case M.lookup d cache of
    Just vs -> return vs
    Nothing -> case rule d of
      Zero -> cacheFirst d $ fmap (1 :) $ blinking 1
      Mul -> cacheFirst d $ fmap (1 :) $ blinking (d * 2024)
      Split -> cacheFirst d $ do
        let (l, r) = split d
        tailL <- blinking l
        tailR <- blinking r
        let seq = 1 : zipWith (+) tailL tailR
        return seq

blinkTimes :: Int -> [Int] -> Int
blinkTimes n ls = let m = (mapM blinking ls) in sum $ map (!! n) $ evalState m M.empty

main :: IO ()
main = putStrLn $ show $ (blinkTimes 25 initialStones, blinkTimes 75 initialStones)
