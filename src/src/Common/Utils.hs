module Common.Utils where

import qualified Data.Text as T

switchMaybeAndList :: [Maybe a] -> Maybe [a]
switchMaybeAndList = foldr go (Just [])
  where
    go (Just u) (Just l) = Just $ l ++ [u]
    go Nothing _ = Nothing


separateToken :: T.Text -> Maybe T.Text
separateToken headerValue = case T.splitOn " " headerValue of
  ("Bearer" : token : _) -> Just token
  _ -> Nothing