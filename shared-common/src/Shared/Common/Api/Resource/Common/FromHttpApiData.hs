module Shared.Common.Api.Resource.Common.FromHttpApiData where

import qualified Data.Text as T
import qualified Data.UUID as U
import Servant.API
import Text.Read (readMaybe)

import Shared.Common.Util.String

instance FromHttpApiData [String] where
  parseQueryParam = Right . splitOn "," . T.unpack

instance FromHttpApiData [U.UUID] where
  parseQueryParam = genericParseQueryParams

genericParseQueryParam :: Read a => T.Text -> Either T.Text a
genericParseQueryParam x =
  case readMaybe (T.unpack x) of
    Just phase -> Right phase
    Nothing -> Left . T.pack . f' "Unable to parse %s" $ [T.unpack x]

genericParseQueryParams :: Read a => T.Text -> Either T.Text [a]
genericParseQueryParams xs =
  let foldFn :: Read a => Either T.Text [a] -> String -> Either T.Text [a]
      foldFn (Left err) _ = Left err
      foldFn (Right acc) x =
        case readMaybe x of
          Just sharing -> Right $ sharing : acc
          Nothing -> Left . T.pack . f' "Unable to parse %s" $ [x]
   in foldl foldFn (Right []) . splitOn "," . T.unpack $ xs
