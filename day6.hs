import Control.Monad.Identity
import Data.Array
import Data.List
import Data.Set qualified as Set
import Data.Traversable
import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("Got: " ++ show a) a

-- stupid; use Array (Int, Int) Char -- see day 8
type Input = Array Int (Array Int Char)

infix 8 <|

-- | Get character at position
(<|) :: Input -> Pos -> Maybe Char
(<|) i (r, c, _) =
  if inRange (bounds i) r && inRange (bounds $ i ! r) c then Just (i ! r ! c) else Nothing

infix 8 ##

-- | Put obstacle at position
(##) :: Input -> Pos -> Input
(##) i (r, c, _) = i // [(r, i ! r // [(c, '#')])]

data Dir = N | E | W | S deriving (Show, Eq, Ord)

-- | Represents guard position - (vertica index, horizontal index, moving direction)
type Pos = (Int, Int, Dir)

infix 4 ===

-- | Quotient by identifying all directions
(===) :: Pos -> Pos -> Bool
(===) (r1, c1, _) (r2, c2, _) = (r1, c1) == (r2, c2)

-- | Clockwise rotation
rotate :: Pos -> Pos
rotate (r, c, N) = (r, c, E)
rotate (r, c, E) = (r, c, S)
rotate (r, c, S) = (r, c, W)
rotate (r, c, W) = (r, c, N)

forward :: Pos -> Pos
forward (r, c, N) = (r - 1, c, N)
forward (r, c, E) = (r, c + 1, E)
forward (r, c, S) = (r + 1, c, S)
forward (r, c, W) = (r, c - 1, W)

-- | Returns (line count, line length).
-- Expects all lines to be same length, and there to be at least one line.
dimensions :: String -> (Int, Int)
dimensions contents =
  let ls = lines contents
   in (length ls, length $ head ls)

-- | Expects all lines to be same length, and there to be at least one line.
inputFromString :: String -> Input
inputFromString contents =
  listArray (0, lineCount - 1) $ map (\l -> listArray (0, lineLength - 1) l) $ lines contents
  where
    (lineCount, lineLength) = dimensions contents

enumerateInput :: Input -> [(Int, Int, Char)]
enumerateInput input = do
  (rowIndex, row) <- assocs input
  val <- map (\(ci, ch) -> (rowIndex, ci, ch)) $ assocs row
  return val

initialPos :: Input -> Pos
initialPos input =
  let (rowIndex, colIndex, _) = surely pos in (rowIndex, colIndex, N)
  where
    pos = find (\(_, _, ch) -> ch == '^') $ enumerateInput input
    surely = maybe undefined id

-- | Nothing means position out of bound
nextPos :: Input -> Pos -> Maybe Pos
nextPos i p = do
  forwardChar <- i <| (forward p)
  if forwardChar == '#' then nextPos i (rotate p) else Just (forward p)

-- no cycle detection !!
guardPath :: Input -> Pos -> [Pos]
guardPath i p =
  guardPathAcc i [p]
  where
    guardPathAcc :: Input -> [Pos] -> [Pos]
    guardPathAcc i ps = maybe (ps) (\n -> guardPathAcc i (n : ps)) (nextPos i $ head ps)

pathUniquePositions :: Input -> Pos -> [Pos]
pathUniquePositions i p = (nubBy (===)) $ guardPath i p

pathCycles :: Input -> Pos -> Bool
pathCycles i p =
  pathCyclesAcc i p Set.empty
  where
    pathCyclesAcc :: Input -> Pos -> (Set.Set Pos) -> Bool
    pathCyclesAcc i p s =
      if (Set.member p s)
        then True
        else maybe False (\n -> pathCyclesAcc i n $ Set.insert p s) (nextPos i p)

withCount :: [a] -> [(Int, a)]
withCount =
  withCountH 0
  where
    withCountH n (a : as) = (n, a) : (withCountH (n + 1) as)

countObstaclePositions :: Input -> Int
countObstaclePositions i = runIdentity $ do
  let init = initialPos i
  -- we only need to test putting obstructions at original path
  -- starting from *second* position
  let candidates = pathUniquePositions i $ maybe undefined id $ nextPos i init
  let passing = map (\c -> pathCycles (i ## debug c) init) candidates

  return $ length $ filter id $ passing

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  let input = inputFromString contents
  let position = initialPos input
  putStrLn $ (++) "Path length: " $ show $ length $ pathUniquePositions input position
  putStrLn $ (++) "Possible obstacles positions: " $ show $ countObstaclePositions input
