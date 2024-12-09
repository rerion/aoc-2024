import Data.Array
import Data.List
import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("Got: " ++ show a) a

type Index = (Int, Int)

type Arr = Array Index Char

withinBounds :: Arr -> Index -> Bool
withinBounds = inRange . bounds

arrToString :: Arr -> String
arrToString arr = unlines ls
  where
    ls = map (map snd) $ groupBy (\x -> \y -> (fst $ fst x) == (fst $ fst y)) $ assocs arr

arrFromString :: String -> Arr
arrFromString s = array bounds entries
  where
    ls = lines s
    lineCount = length ls
    lineLength = length $ head ls
    bounds = ((0, 0), (lineCount - 1, lineLength - 1))
    entries = [((i, j), ls !! i !! j) | i <- [0 .. lineCount - 1], j <- [0 .. lineLength - 1]]

pairFrequencies :: Arr -> [(Index, Index)]
pairFrequencies arr = [(i, j) | (i, ch1) <- assocs arr, (j, ch2) <- assocs arr, j > i, ch1 == ch2, ch1 /= '.', ch1 /= '#']

antinodes :: Arr -> [Index]
antinodes arr = nub [i | p <- pairFrequencies arr, i <- antinodesForPair p]
  where
    antinodesForPair :: (Index, Index) -> [Index]
    antinodesForPair ((r1, c1), (r2, c2)) = let (dr, dc) = (r2 - r1, c2 - c1) in filter (withinBounds arr) [(r2 + dr, c2 + dc), (r1 - dr, c1 - dc)]

antinodes2 :: Arr -> [Index]
antinodes2 arr = nub [i | p <- pairFrequencies arr, i <- antinodesForPair p]
  where
    antinodesForPair :: (Index, Index) -> [Index]
    antinodesForPair (i, j) = let delta = diff (i, j) in inLineAcc (firstInLine i delta) delta []
    diff :: (Index, Index) -> (Int, Int)
    diff ((r1, c1), (r2, c2)) = (r2 - r1, c2 - c1)
    firstInLine :: Index -> (Int, Int) -> Index
    firstInLine (r, c) (dr, dc) = if not $ withinBounds arr (r - dr, c - dc) then (r, c) else firstInLine (r - dr, c - dc) (dr, dc)
    inLineAcc :: Index -> (Int, Int) -> [Index] -> [Index]
    inLineAcc (r, c) (dr, dc) acc = if withinBounds arr (r, c) then inLineAcc (r + dr, c + dc) (dr, dc) ((r, c) : acc) else acc

markAntinodes :: Arr -> Arr
markAntinodes arr = arr // (map (\x -> (x, '#')) $ antinodes2 arr)

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  let input = arrFromString contents
  let part1 = length $ antinodes input
  let part2 = length $ antinodes2 input
  putStrLn $ show $ (part1, part2)
