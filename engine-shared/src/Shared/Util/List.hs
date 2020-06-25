module Shared.Util.List where

import Data.Either (partitionEithers)
import qualified Data.List as L

groupBy :: Ord a => (a -> a -> Bool) -> [a] -> [[a]]
groupBy fn = L.groupBy fn . L.sort

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
  where
    rdHelper seen [] = seen
    rdHelper seen (x:xs)
      | x `elem` seen = rdHelper seen xs
      | otherwise = rdHelper (seen ++ [x]) xs

elems :: Eq a => [a] -> [a] -> Bool
elems (x:xs) list = x `elem` list && xs `elems` list
elems [] list = True

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (x:xs) = xs

generateList :: Int -> [Int]
generateList size = [0 .. (size - 1)]

foldEither :: [Either l r] -> Either l [r]
foldEither eitherList =
  case partitionEithers eitherList of
    (l:_, rs) -> Left l
    (_, rs) -> Right rs

foldMaybe :: [Maybe a] -> Maybe [a]
foldMaybe = foldl go (Just [])
  where
    go (Just l) (Just u) = Just $ l ++ [u]
    go _ Nothing = Nothing
    go Nothing _ = Nothing

foldInContext :: Monad monad => [monad a] -> monad [a]
foldInContext = Prelude.foldl foldOne (return [])
  where
    foldOne :: Monad monad => monad [a] -> monad a -> monad [a]
    foldOne listIO entityIO = do
      list <- listIO
      entity <- entityIO
      return $ list ++ [entity]
