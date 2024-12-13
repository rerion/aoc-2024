import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.Set qualified as S
import Debug.Trace

debug :: (Show a) => String -> a -> a
debug s a = trace (s ++ ": " ++ show a) a

type Index = (Int, Int)

type Arr = Array Index Char

type Components = [[Index]]

withinBounds :: Arr -> Index -> Bool
withinBounds = inRange . bounds

arrToString :: Arr -> String
arrToString arr = unlines $ ls
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

connectedComponents :: Arr -> Components
connectedComponents arr = components ixs []
  where
    ixs :: S.Set Index
    ixs = S.fromList (indices arr)

    components :: S.Set Index -> [[Index]] -> [[Index]]
    components ixs acc =
      if S.null ixs
        then acc
        else
          let next = S.findMin ixs
           in let (c, nixs) = component (arr ! next) ixs [next] [] in components nixs (c : acc)

    component :: Char -> S.Set Index -> [Index] -> [Index] -> ([Index], S.Set Index)
    component _ unvisited [] acc = (nub acc, unvisited)
    component c unvisited (q : qs) acc =
      let nbors = neighbours c q unvisited
       in component c (S.filter (not . (`elem` q : nbors)) unvisited) (qs ++ nbors) (q : acc)

    neighbours :: Char -> Index -> S.Set Index -> [Index]
    neighbours ch (r, c) ixs =
      filter
        (\i -> S.member i ixs && arr ! i == ch)
        [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

componentSize :: Arr -> [Index] -> (Int, Int)
componentSize arr c = componentSizeH c (0, 0)
  where
    componentSizeH :: [Index] -> (Int, Int) -> (Int, Int)
    componentSizeH [] acc = acc
    componentSizeH (i : is) (a, p) =
      let per = indexPerimeter i
       in componentSizeH is (a + 1, p + per)

    indexPerimeter :: Index -> Int
    indexPerimeter (r, c) =
      let neighbours = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
       in length $ filter (\i -> (withinBounds arr i && arr ! i /= arr ! (r, c)) || (not $ withinBounds arr i)) neighbours

data SideDir = L | R | U | D deriving (Eq, Show)

-- stupid; there is a bijection corners ~ sides
componentSize2 :: Arr -> [Index] -> (Int, Int)
componentSize2 arr c = (length c, sides c [])
  where
    char :: Char
    char = arr ! (head c)

    sides :: [Index] -> [(Index, SideDir)] -> Int
    sides [] acc = length acc
    sides (x : xs) acc =
      let ss = map (x,) (sideDirs x)
       in let new = filter (not . sideIncluded acc) ss
           in sides xs (new ++ acc)

    sideIncluded :: [(Index, SideDir)] -> (Index, SideDir) -> Bool
    sideIncluded arr i = maybe False (const True) $ find (areOnSameSide i) arr

    sideDirs :: Index -> [SideDir]
    sideDirs i = filter (\s -> isSide s i) [L, R, U, D]

    areOnSameSide :: (Index, SideDir) -> (Index, SideDir) -> Bool
    areOnSameSide (i1, d1) (i2, d2) = d1 == d2 && areOnSide i1 i2 d1

    areOnSide :: Index -> Index -> SideDir -> Bool
    areOnSide (r1, c1) (r2, c2) U = let line = map (r1,) $ symRange c1 c2 in r1 == r2 && all (isSide U) line
    areOnSide (r1, c1) (r2, c2) D = let line = map (r1,) $ symRange c1 c2 in r1 == r2 && all (isSide D) line
    areOnSide (r1, c1) (r2, c2) L = let line = map (,c1) $ symRange r1 r2 in c1 == c2 && all (isSide L) line
    areOnSide (r1, c1) (r2, c2) R = let line = map (,c1) $ symRange r1 r2 in c1 == c2 && all (isSide R) line

    symRange :: Int -> Int -> [Int]
    symRange a b
      | a <= b = range (a, b)
      | otherwise = range (b, a)

    isSide :: SideDir -> Index -> Bool
    isSide L (r, c) = ((not $ withinBounds arr (r, c - 1)) || (arr ! (r, c - 1) /= arr ! (r, c))) && arr ! (r, c) == char
    isSide R (r, c) = ((not $ withinBounds arr (r, c + 1)) || (arr ! (r, c + 1) /= arr ! (r, c))) && arr ! (r, c) == char
    isSide U (r, c) = ((not $ withinBounds arr (r - 1, c)) || (arr ! (r - 1, c) /= arr ! (r, c))) && arr ! (r, c) == char
    isSide D (r, c) = ((not $ withinBounds arr (r + 1, c)) || (arr ! (r + 1, c) /= arr ! (r, c))) && arr ! (r, c) == char

printComponent :: Arr -> [Index] -> IO ()
printComponent arr c = do
  let sh = arrToString $ listArray (bounds arr) (repeat '.') // map (\i -> (i, '*')) c
  putStrLn $ sh
  putStrLn $ show $ componentSize arr c
  putStrLn $ show $ componentSize2 arr c

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  let input = arrFromString contents
  let components = connectedComponents input
  let total = foldr (\(a, b) -> \acc -> acc + a * b) 0 $ map (componentSize input) components
  let total2 = foldr (\(a, b) -> \acc -> acc + a * b) 0 $ map (componentSize2 input) components
  -- forM_ components $ printComponent input
  putStrLn $ show (total, total2)
