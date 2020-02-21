module Shared.Util.Token where

import Shared.Util.String (splitOn)

separateToken :: String -> Maybe String
separateToken headerValue =
  case splitOn " " headerValue of
    ["Bearer", token] ->
      if token == ""
        then Nothing
        else Just token
    _ -> Nothing
