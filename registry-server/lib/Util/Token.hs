module Util.Token where

import qualified Data.Text as T

separateToken :: T.Text -> Maybe T.Text
separateToken headerValue =
  case T.splitOn " " headerValue of
    ("Bearer":token:[]) ->
      if token == ""
        then Nothing
        else Just token
    _ -> Nothing
