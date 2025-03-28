module Shared.Common.Util.List where

import Data.Either (partitionEithers)
import qualified Data.List as L

addIfNotExists :: Eq a => a -> [a] -> [a]
addIfNotExists x list =
  case L.find (== x) list of
    Just _ -> list
    Nothing -> list ++ [x]

groupBy :: Ord a => (a -> a -> Bool) -> [a] -> [[a]]
groupBy fn = L.groupBy fn . L.sort

groupBy' :: Eq field => (a -> field) -> (b -> field) -> [a] -> [b] -> [(a, b)]
groupBy' xFn yFn xs ys =
  let fn acc x =
        case L.find (\y -> yFn y == xFn x) ys of
          Just y -> (x, y) : acc
          Nothing -> acc
   in foldl fn [] xs

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
  where
    rdHelper seen [] = seen
    rdHelper seen (x : xs)
      | x `elem` seen = rdHelper seen xs
      | otherwise = rdHelper (seen ++ [x]) xs

elems :: Eq a => [a] -> [a] -> Bool
elems (x : xs) list = x `elem` list && xs `elems` list
elems [] list = True

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x : _) = Just x

lastSafe :: [a] -> Maybe a
lastSafe [] = Nothing
lastSafe xs = Just . last $ xs

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (x : xs) = xs

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) =
  x
    : if p x
      then takeWhileInclusive p xs
      else []

dropWhileInclusive :: (a -> Bool) -> [a] -> [a]
dropWhileInclusive _ [] = []
dropWhileInclusive p (x : xs) =
  if p x
    then dropWhileInclusive p xs
    else x : xs

dropWhileExclusive :: (a -> Bool) -> [a] -> [a]
dropWhileExclusive _ [] = []
dropWhileExclusive p (x : xs) =
  if p x
    then dropWhileExclusive p xs
    else xs

generateList :: Int -> [Int]
generateList size = [0 .. (size - 1)]

foldEither :: [Either l r] -> Either l [r]
foldEither eitherList =
  case partitionEithers eitherList of
    (l : _, rs) -> Left l
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

replace :: (Eq a) => a -> a -> [a] -> [a]
replace new old = L.map replace'
  where
    replace' elem = if elem == old then new else elem
