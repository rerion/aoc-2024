import Data.Array
import Data.Char
import Data.List
import Debug.Trace

debug :: (Show a) => a -> a
debug a = trace ("Got: " ++ show a) a

type Arr = Array Index Length

type ID = Int

type Index = Int

type Offset = Int

type Length = Int

data Consuming = File | Slot deriving (Eq, Show)

data State = State
  { forwardIndex :: Index,
    forwardConsumed :: Length,
    forwardTotal :: Length,
    backwardIndex :: Index,
    backwardConsumed :: Length,
    backwardTotal :: Length,
    consuming :: Consuming,
    acc :: Int,
    pointer :: Offset
  }
  deriving (Show)

loadInput :: String -> Arr
loadInput s = listArray bounds $ map digitToInt s
  where
    len = length s
    bounds = (0, len - 1)

-- gets value from file blocks of given length, id and starting at given index
value :: Offset -> ID -> Length -> Int
value ix id l = id * (2 * ix + l - 1) * l `div` 2

idAt :: Index -> ID
idAt = (`div` 2)

-- odd number of elements -> last digit is file
initState :: Arr -> State
initState arr =
  State
    { forwardIndex = 0,
      forwardConsumed = 0,
      forwardTotal = arr ! 0,
      pointer = 0,
      backwardIndex = snd (bounds arr),
      backwardConsumed = 0,
      backwardTotal = arr ! snd (bounds arr),
      consuming = File,
      acc = 0
    }

consumeSlot :: State -> Arr -> State
consumeSlot s arr
  | slotSpaceLeft >= tailLengthLeft =
      s -- consume rest of tail file, still consuming slot
        { acc = acc s + tailValue tailLengthLeft,
          pointer = pointer s + tailLengthLeft,
          backwardConsumed = 0,
          backwardTotal = arr ! (backwardIndex s - 2),
          backwardIndex = backwardIndex s - 2,
          forwardConsumed = forwardConsumed s + tailLengthLeft
        }
  | otherwise =
      s -- consume rest of space and move to next file
        { acc = acc s + tailValue slotSpaceLeft,
          pointer = pointer s + slotSpaceLeft,
          backwardConsumed = backwardConsumed s + slotSpaceLeft,
          forwardConsumed = 0,
          forwardTotal = arr ! (forwardIndex s + 1),
          forwardIndex = forwardIndex s + 1,
          consuming = File
        }
  where
    slotSpaceLeft = forwardTotal s - forwardConsumed s
    tailLengthLeft = backwardTotal s - backwardConsumed s
    tailValue len = value (pointer s) (idAt $ backwardIndex s) len

consumeFile :: State -> Arr -> State
consumeFile s arr =
  s -- consume file, move to next slot
    { acc = acc s + fileValue,
      pointer = pointer s + forwardTotal s,
      forwardConsumed = 0,
      forwardTotal = arr ! (forwardIndex s + 1),
      forwardIndex = forwardIndex s + 1,
      consuming = Slot
    }
  where
    fileValue = value (pointer s) (idAt $ forwardIndex s) (forwardTotal s)

-- overly complicated but fast
checksum :: State -> Arr -> Int
checksum s arr
  | halting = acc s + remainder
  | consuming s == File = checksum (consumeFile s arr) arr
  | consuming s == Slot = checksum (consumeSlot s arr) arr
  where
    halting = backwardIndex s <= forwardIndex (debug s)
    remainder = value (pointer s) (idAt $ backwardIndex s) (backwardTotal s - backwardConsumed s)

main :: IO ()
main = do
  contents <- readFile "./input2.txt"
  let input = loadInput contents
  putStrLn $ show $ checksum (initState input) input
