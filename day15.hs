import Control.Monad
import Data.Array
import Data.Either
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

type Index = (Int, Int)

type Map = Array Index Object

-- up, down, left, right
data Movement = U | D | L | R deriving (Show, Eq)

-- player (robot), space, wall, box
data Object = P | S | W | B deriving (Eq)

instance Show Object where
  show P = "@"
  show S = "."
  show W = "#"
  show B = "O"

data Object2 = Pp | Ss | Ww | LB | RB deriving (Eq)

instance Show Object2 where
  show Pp = "@"
  show Ss = "."
  show Ww = "#"
  show LB = "["
  show RB = "]"

type Map2 = Array Index Object2

parser :: Parser (Map, Index, [Movement])
parser = transformResult <$> parseMap <* newline <*> parseMovements
  where
    parseMap :: Parser [[Object]]
    parseMap = many1 parseMapRow
    parseMapRow = many1 parseMapObject <* char '\n'
    parseMapObject = transformMapObject <$> oneOf "O.#@"
    transformMapObject c
      | c == '0' = B
      | c == '.' = S
      | c == '#' = W
      | c == '@' = P
      | otherwise = B

    parseMovements :: Parser [Movement]
    parseMovements = many1 $ transformMovement <$> oneOf "v>^<" <* optional newline
    transformMovement c
      | c == 'v' = D
      | c == '>' = R
      | c == '<' = L
      | c == '^' = U

    mapBounds :: [[Object]] -> (Index, Index)
    mapBounds o = (,) (0, 0) (length o - 1, length (head o) - 1)

    findPlayerIndex :: Map -> (Index)
    findPlayerIndex arr = fst $ fromJust $ find ((== P) . snd) $ assocs arr

    transformResult :: [[Object]] -> [Movement] -> (Map, Index, [Movement])
    transformResult os ms = let arr = listArray (mapBounds os) (concat os) in (arr, findPlayerIndex arr, ms)

printMap :: (Show o) => Array Index o -> String
printMap arr = unlines $ ls
  where
    ls = map (concat . map (show . snd)) $ groupBy (\x -> \y -> (fst $ fst x) == (fst $ fst y)) $ assocs arr

(|>) :: Index -> Movement -> Index
(|>) (r, c) U = (r - 1, c)
(|>) (r, c) D = (r + 1, c)
(|>) (r, c) L = (r, c - 1)
(|>) (r, c) R = (r, c + 1)

push :: Map -> Index -> Movement -> Maybe Map
push m i d =
  let nextPos = i |> d
   in case m ! nextPos of
        S -> Just $ m // [(nextPos, m ! i), (i, S)]
        W -> Nothing
        B -> do
          recur <- push m nextPos d
          return $ recur // [(nextPos, m ! i), (i, S)]

makeMove :: Map -> Index -> Movement -> (Map, Index)
makeMove m i d = case push m i d of
  Just mm -> (mm, i |> d)
  Nothing -> (m, i)

makeMoves :: Map -> Index -> [Movement] -> (Map, Index)
makeMoves m i [] = (m, i)
makeMoves m i (d : ds) = let (mm, ii) = makeMove m i d in makeMoves mm ii ds

boxPositions :: Map -> [Index]
boxPositions = map fst . filter ((B ==) . snd) . assocs

gpsSum :: [Index] -> Int
gpsSum = sum . map (\(r, c) -> 100 * r + c)

double :: Object -> [Object2]
double W = [Ww, Ww]
double S = [Ss, Ss]
double P = [Pp, Ss]
double B = [LB, RB]

doubleMap :: Map -> Map2
doubleMap m = listArray newBounds (concatMap double $ elems m)
  where
    newBounds = (fst $ bounds m, (fst . snd $ bounds m, 2 * (snd . snd $ bounds m) + 1))

push2 :: Map2 -> Index -> Movement -> Maybe Map2
push2 m i d = if pushingBoxVertically then pushBoxVertical else pushUnit m i d
  where
    pushingBoxVertically = (m ! i == LB || m ! i == RB) && (d == U || d == D)

    boxCoords :: (Index, Index)
    boxCoords
      | m ! i == LB = (i, i |> R)
      | m ! i == RB = (i |> L, i)

    (boxLeft, boxRight) = boxCoords

    pushUnit :: Map2 -> Index -> Movement -> Maybe Map2
    pushUnit m i d = case m ! (i |> d) of
      Ww -> Nothing
      Ss -> Just $ m // [(i |> d, m ! i), (i, Ss)]
      u -> do
        recur <- push2 m (i |> d) d
        return $ recur // [(i |> d, m ! i), (i, Ss)]

    pushBoxVertical :: Maybe Map2
    pushBoxVertical = do
      recL <- pushUnit m boxLeft d
      recR <- pushUnit recL boxRight d
      return $ recR // [(boxLeft |> d, LB), (boxRight |> d, RB), (boxLeft, Ss), (boxRight, Ss)]

makeMove2 :: Map2 -> Index -> Movement -> (Map2, Index)
makeMove2 m i d = case push2 m i d of
  Just mm -> (mm, i |> d)
  Nothing -> (m, i)

makeMoves2 :: Map2 -> Index -> [Movement] -> (Map2, Index)
makeMoves2 m i [] = (m, i)
makeMoves2 m i (d : ds) = let (mm, ii) = makeMove2 m i d in makeMoves2 mm ii ds

boxPositions2 :: Map2 -> [Index]
boxPositions2 = map fst . filter ((LB ==) . snd) . assocs

maps2 :: Map2 -> Index -> [Movement] -> [(Map2, Index)]
maps2 m i ds = (m, i) : maps2Acc m i ds []
  where
    maps2Acc m i [] acc = reverse acc
    maps2Acc m i (d : ds) acc = let (mm, ii) = makeMove2 m i d in maps2Acc mm ii ds ((mm, ii) : acc)

main :: IO ()
main = do
  (map, player, movements) <- (fromRight undefined) <$> parseFromFile parser "./input.txt"

  let (resMap, _) = makeMoves map player movements
  let sum1 = gpsSum $ boxPositions resMap

  let (map2, player2) = (doubleMap map, (fst player, 2 * snd player))

  let (resMap2, _) = makeMoves2 map2 player2 movements
  let sum2 = gpsSum $ boxPositions2 resMap2

  putStr $ printMap resMap
  putStrLn $ show sum1

  putStr $ printMap resMap2
  putStrLn $ show sum2
