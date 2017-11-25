module Common.Utils where

import qualified Data.Text as T

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
