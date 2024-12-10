import Data.Array
import Data.Char
import Data.List
import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("Got: " ++ show a) a

-- pullback :: (Eq z) => (a -> z) -> (b -> z) -> [a] -> [b] -> [(a,b)]

type Index = (Int, Int)

type Arr = Array Index Int

withinBounds :: Arr -> Index -> Bool
withinBounds = inRange . bounds

arrToString :: Arr -> String
arrToString arr = unlines $ ls
  where
    ls = map (map $ intToDigit . snd) $ groupBy (\x -> \y -> (fst $ fst x) == (fst $ fst y)) $ assocs arr

arrFromString :: String -> Arr
arrFromString s = array bounds entries
  where
    ls = lines s
    lineCount = length ls
    lineLength = length $ head ls
    bounds = ((0, 0), (lineCount - 1, lineLength - 1))
    entries = [((i, j), digitToInt $ ls !! i !! j) | i <- [0 .. lineCount - 1], j <- [0 .. lineLength - 1]]

zeros :: Arr -> [(Index, Int)]
zeros = (filter (\n -> snd n == 0)) . assocs

nextDigits :: Arr -> (Index, Int) -> [(Index, Int)]
nextDigits arr ((r, c), d) = map (\i -> (i, arr ! i)) next
  where
    sorrounding = filter (inRange (bounds arr)) [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
    next = filter (\i -> arr ! i == d + 1) sorrounding

isEnd :: (Index, Int) -> Bool
isEnd (_, d) = d == 9

continueTrail :: Arr -> [(Index, Int)] -> [[(Index, Int)]]
continueTrail arr (s : ss) = [(n : s : ss) | n <- nextDigits arr s]

completeTrail :: Arr -> [(Index, Int)] -> [[(Index, Int)]]
completeTrail arr (s : ss)
  | isEnd s = pure (s : ss)
  | otherwise = do
      cont <- continueTrail arr (s : ss)
      completeTrail arr cont

trails :: Arr -> [[(Index, Int)]]
trails arr = [x | start <- zeros arr, x <- completeTrail arr [start]]

trailEndpoints :: [(Index, Int)] -> (Index, Index)
trailEndpoints t = (fst $ head t, fst $ last t)

score :: [[(Index, Int)]] -> Int
score = sum . (map length) . (groupBy (\x -> \y -> snd x == snd y)) . nub . (map trailEndpoints)

score2 :: [[(Index, Int)]] -> Int
score2 = sum . (map length) . (groupBy (\x -> \y -> snd x == snd y)) . (map trailEndpoints)

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  let input = arrFromString contents
  let ts = trails input
  putStrLn $ show $ (score ts, score2 ts)
