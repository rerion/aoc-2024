import Control.Monad.State
import Data.Array
import Data.Char
import Data.List
import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("Got: " ++ show a) a

type Arr = Array Index (ID, Offset, Length)

type Index = Int

type ID = Int

type Offset = Int

type Length = Int

loadInput :: String -> Arr
loadInput s = listArray bounds $ process s 0 0 True
  where
    len = length s
    bounds = (0, len - 1)
    process :: String -> Offset -> ID -> Bool -> [(ID, Offset, Length)]
    process "" _ _ _ = []
    process (x : xs) offset id f = head : tail
      where
        len = digitToInt x
        head = (if f then id else -1, offset, len)
        tail = process xs (offset + len) (if f then id else id + 1) (not f)

-- gets value from file blocks of given length, id and starting at given index
value :: ID -> Offset -> Length -> Int
value id off l = id * (2 * off + l - 1) * l `div` 2

slots :: Arr -> [(ID, Offset, Length)]
slots arr = sort $ filter (\(id, _, len) -> id < 0 && len > 0) $ elems arr

blocksReverse :: Arr -> [(ID, Offset, Length)]
blocksReverse arr = map (arr !) [h, h - 2 .. l]
  where
    (l, h) = bounds arr

processBlocks :: [(ID, Offset, Length)] -> State [(ID, Offset, Length)] Int
processBlocks bs = fmap sum $ mapM processBlock bs

processBlock :: (ID, Offset, Length) -> State [(ID, Offset, Length)] Int
processBlock (id, o, l) = do
  slots <- get
  let slot = find (\(_, off, len) -> len >= l && off < o) (fst (slots, (id, o, l)))
  case slot of
    Nothing -> return $ value id o l
    Just (_, so, sl) -> do
      if sl - l > 0
        then
          put $ (insert (-1, so + l, sl - l) . delete (-1, so, sl)) slots
        else
          put $ delete (-1, so, sl) slots
      return $ value id so l

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  let input = loadInput contents
  let result = evalState (processBlocks $ blocksReverse input) (slots input)
  putStrLn $ show result
