module Cache where

import Data.Map qualified as Map

newtype Cache k v s = Cache ((Map.Map k v) -> (Map.Map k v, s)) deriving (Functor)

instance (Ord k) => Applicative (Cache k v) where
  pure v = Cache $ \c -> (c, v)
  f <*> x = do
    xx <- x
    ff <- f
    return $ ff xx

instance (Ord k) => Monad (Cache k v) where
  (Cache getA) >>= f = Cache $ \c -> let (cc, res) = getA c in let (Cache ff) = f res in ff cc

get :: (Ord k) => k -> Cache k v (Maybe v)
get k = Cache $ \m -> if Map.member k m then (m, Just $ m Map.! k) else (m, Nothing)

set :: (Ord k) => k -> v -> Cache k v ()
set k v = Cache $ \m -> (Map.insert k v m, ())

runWithCache :: (Ord k) => Cache k v s -> s
runWithCache (Cache f) = snd $ f (Map.empty)
