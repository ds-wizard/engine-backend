module Shared.Common.Util.Map where

import qualified Data.Map.Strict as M

insertFlipped value key = M.insert key value

doubleGroupBy :: (Ord a, Ord key1, Ord key2) => (a -> key1) -> (a -> key2) -> (a -> value) -> [[a]] -> M.Map key1 (M.Map key2 value)
doubleGroupBy mappingKey1Fn mappingKey2Fn mappingValueFn xxs =
  let go acc xs =
        case M.lookup (mappingKey1Fn . head $ xs) acc of
          Just m -> M.insert (mappingKey1Fn . head $ xs) (M.insert (mappingKey2Fn $ xs !! 1) (mappingValueFn $ xs !! 2) m) acc
          Nothing -> M.insert (mappingKey1Fn . head $ xs) (M.singleton (mappingKey2Fn $ xs !! 1) (mappingValueFn $ xs !! 2)) acc
   in foldl go M.empty xxs
