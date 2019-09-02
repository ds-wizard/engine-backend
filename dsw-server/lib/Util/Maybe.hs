module Util.Maybe where

concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe (Just (Just a)) = Just a
concatMaybe _ = Nothing
