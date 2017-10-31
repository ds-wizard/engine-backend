module Common.Utils where

switchMaybeAndList :: [Maybe a] -> Maybe [a]
switchMaybeAndList = foldr go (Just [])
  where
    go (Just u) (Just l) = Just $ l ++ [u]
    go Nothing _ = Nothing
