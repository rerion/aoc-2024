import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.Either
import Debug.Trace
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

type Position = (Int, Int)

type Velocity = (Int, Int)

type BathroomSize = (Int, Int)

debug :: (Show a) => String -> a -> a
debug s a = trace (s ++ ": " ++ show a) a

parseInt :: Parser Int
parseInt = sign <*> (read <$> many1 digit)
  where
    sign :: Parser (Int -> Int)
    sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

parsePosition :: Parser Position
parsePosition = (,) <$> (string "p=" *> parseInt) <*> (char ',' *> parseInt)

parseVelocity :: Parser Velocity
parseVelocity = (,) <$> (string "v=" *> parseInt) <*> (char ',' *> parseInt)

parseWhitespace :: Parser ()
parseWhitespace = (const ()) <$> (many $ oneOf " \n\t")

parseRobot :: Parser (Position, Velocity)
parseRobot = (,) <$> (parsePosition <* parseWhitespace) <*> (parseVelocity <* parseWhitespace)

parseInput :: Parser [(Position, Velocity)]
parseInput = many parseRobot

robotNthState :: BathroomSize -> Int -> (Position, Velocity) -> (Position, Velocity)
robotNthState (w, h) n ((px, py), (vx, vy)) = (((px + n * vx) `mod` w, (py + n * vy) `mod` h), (vx, vy))

robotPositions :: BathroomSize -> Int -> [(Position, Velocity)] -> [Position]
robotPositions s n = map (fst . robotNthState s n)

-- (NW, NE, SE, SW)
countInQuadrants :: BathroomSize -> [Position] -> (Int, Int, Int, Int)
countInQuadrants (w, h) = countInQuadrantsAcc (0, 0, 0, 0)
  where
    midX = w `div` 2
    midY = h `div` 2

    countInQuadrantsAcc :: (Int, Int, Int, Int) -> [Position] -> (Int, Int, Int, Int)
    countInQuadrantsAcc acc [] = acc
    countInQuadrantsAcc acc (h : t) = countInQuadrantsAcc (addToAcc acc h) t

    addToAcc :: (Int, Int, Int, Int) -> Position -> (Int, Int, Int, Int)
    addToAcc (nw, ne, se, sw) (x, y)
      | x > midX && y > midY = (nw, ne, se + 1, sw)
      | x < midX && y > midY = (nw, ne, se, sw + 1)
      | x < midX && y < midY = (nw + 1, ne, se, sw)
      | x > midX && y < midY = (nw, ne + 1, se, sw)
      | otherwise = (nw, ne, se, sw)

safetyFactor :: (Int, Int, Int, Int) -> Int
safetyFactor (a, b, c, d) = a * b * c * d

-- until init repeats
allFactors :: BathroomSize -> [(Position, Velocity)] -> [Int]
allFactors s rs = (init :) $ map fst $ takeWhile (/= (init, snd)) $ zip factors (drop 1 factors)
  where
    -- good enough; factor is hash for [positions] / for one index there is a collision
    -- for 2 neighbouring there isn't
    init = safetyFactor $ countInQuadrants s $ robotPositions s 0 rs
    snd = safetyFactor $ countInQuadrants s $ robotPositions s 1 rs
    factors = map (safetyFactor . (countInQuadrants s) . (\t -> robotPositions s t rs)) [1 ..]

main :: IO ()
main = do
  input <- (fromRight []) <$> parseFromFile parseInput "./input.txt"

  forM_ (allFactors (101, 103) input) (putStrLn . show)
