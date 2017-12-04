module Common.Utils where

import qualified Data.Text as T

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

switchMaybeAndList :: [Maybe a] -> Maybe [a]
switchMaybeAndList = foldl go (Just [])
  where
    go (Just l) (Just u) = Just $ l ++ [u]
    go _ Nothing = Nothing
    go Nothing _ = Nothing

separateToken :: T.Text -> Maybe T.Text
separateToken headerValue =
  case T.splitOn " " headerValue of
    ("Bearer":token:[]) ->
      if token == ""
        then Nothing
        else Just token
    _ -> Nothing

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
  where
    rdHelper seen [] = seen
    rdHelper seen (x:xs)
      | x `elem` seen = rdHelper seen xs
      | otherwise = rdHelper (seen ++ [x]) xs