module Shared.Common.Util.Token where

import Shared.Common.Util.String (splitOn)

separateToken :: String -> Maybe String
separateToken headerValue =
  case splitOn " " headerValue of
    ["Bearer", token] ->
      if token == ""
        then Nothing
        else Just token
    _ -> Nothing
