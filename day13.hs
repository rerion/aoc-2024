import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

data Game = Game
  { a :: (Int, Int),
    b :: (Int, Int),
    prize :: (Int, Int)
  }
  deriving (Show)

debug :: (Show a) => String -> a -> a
debug s a = trace (s ++ ": " ++ show a) a

parseButton :: Char -> Parser (Int, Int)
parseButton c = do
  _ <- string $ "Button " ++ c : ": X+"
  deltaX <- many1 digit
  _ <- string ", Y+"
  deltaY <- many1 digit
  return (read deltaX, read deltaY)

parsePrize :: Parser (Int, Int)
parsePrize = do
  _ <- string "Prize: X="
  x <- many1 digit
  _ <- string ", Y="
  y <- many1 digit
  return (read x, read y)

parseGame :: Parser Game
parseGame = do
  a <- parseButton 'A'
  _ <- newline
  b <- parseButton 'B'
  _ <- newline
  prize <- parsePrize
  return Game {a, b, prize}

parseInput :: Parser [Game]
parseInput = many $ do
  g <- parseGame
  _ <- many $ oneOf " \t\n"
  return g

-- naive solution
configValue :: Game -> (Int, Int) -> (Int, Int)
configValue g (c, d) = (c * fst (a g) + d * fst (b g), c * snd (a g) + d * snd (b g))

winningConfigs :: Game -> [(Int, Int)]
winningConfigs g = [(c, d) | c <- [0 .. 100], d <- [0 .. 100], configValue g (c, d) == prize g]

price :: (Int, Int) -> Int
price (a, b) = 3 * a + b

bestPrice :: [(Int, Int)] -> Maybe Int
bestPrice [] = Nothing
bestPrice cfs = Just $ minimum $ map price cfs

-- number theory solution
-- stupid stupid stupid: just solve linear system
--
-- (a,b) -> (x,y,gcd(a,b)) s.t. ax+by=gcd(a,b)
bezoutIdentity :: (Int, Int) -> (Int, Int, Int)
bezoutIdentity (a, b) = lastNontrivial euclideanEqns
  where
    init :: [(Int, Int, Int)]
    init
      | a >= b = [(1, 0, a), (0, 1, b)]
      | otherwise = [(0, 1, b), (1, 0, a)]

    euclideanNext :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    euclideanNext (a1, b1, r1) (a2, b2, r2) = let (q, r) = quotRem r1 r2 in (a1 - q * a2, b1 - q * b2, r)

    euclideanEqns :: [(Int, Int, Int)]
    euclideanEqns = init ++ zipWith euclideanNext euclideanEqns (drop 1 euclideanEqns)

    lastNontrivial :: [(Int, Int, Int)] -> (Int, Int, Int)
    lastNontrivial (x : (a, b, 0) : tail) = x
    lastNontrivial (x : tail) = lastNontrivial tail

-- (a,b) -> t -> (x,y,dx,dy) s.t. (x+kdx)a+(y+kdy)b=t for k integer
solutionSetForm :: (Int, Int) -> Int -> Maybe (Int, Int, Int, Int)
solutionSetForm (a, b) target
  | r == 0 = Just (x * q, y * q, dx, dy)
  | otherwise = Nothing
  where
    (x, y, z) = bezoutIdentity (a, b)
    (q, r) = target `quotRem` z
    dx = b `div` z
    dy = -(a `div` z)

assert :: Bool -> Maybe ()
assert True = Just ()
assert False = Nothing

-- solve for k1 over integers: a+k1da=x+k2dx, b+k1db=y+k2dy; calculate (a+k1da, b+k1db)
simultaneousSolution :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe (Int, Int)
simultaneousSolution (a, b, da, db) (x, y, dx, dy) = do
  let det = dx * db - dy * da
  () <- assert (det /= 0)
  let (k1, r1) = (dx * (y - b) - dy * (x - a)) `quotRem` det
  () <- assert (r1 == 0)
  return (a + k1 * da, b + k1 * db)

solveGame :: Game -> Maybe (Int, Int)
solveGame Game {a, b, prize} = do
  let (t1, t2) = prize
  let (x1, y1) = a
  let (x2, y2) = b
  s1 <- solutionSetForm (x1, x2) t1
  s2 <- solutionSetForm (y1, y2) t2
  (r1, r2) <- simultaneousSolution s1 s2
  () <- assert (r1 >= 0 && r2 >= 0)
  return (r1, r2)

alterPrize :: Game -> Game
alterPrize g = g {prize = (10000000000000 + fst (prize g), 10000000000000 + snd (prize g))}

main :: IO ()
main = do
  contents <- parseFromFile parseInput "./input.txt"
  case contents of
    Left e -> putStrLn $ show e
    Right games -> do
      let wcs = map fromJust $ filter (/= Nothing) $ map (bestPrice . winningConfigs) games
      let wcs2 = map (price . fromJust) $ filter (/= Nothing) $ map (solveGame . alterPrize) games
      putStrLn $ show $ (sum wcs, sum wcs2)
