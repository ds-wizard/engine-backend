module Shared.Common.Api.Resource.Common.SemVer2TupleJM where

import Data.Aeson
import qualified Data.Text as T
import Servant.API
import Text.Read (readMaybe)

import Shared.Common.Model.Common.SemVer2Tuple
import Shared.Common.Util.String

instance FromJSON SemVer2Tuple where
  parseJSON = withText "SemVer2Tuple" $ \versionT ->
    let parts = splitOn "." . T.unpack $ versionT
     in case parts of
          [majorS, minorS] ->
            case (readMaybe majorS, readMaybe minorS) of
              (Just major, Just minor) -> pure $ SemVer2Tuple major minor
              _ -> fail "Invalid SemVer2Tuple format"
          [majorS] ->
            case readMaybe majorS of
              Just major -> pure $ SemVer2Tuple major 0
              _ -> fail "Invalid SemVer2Tuple format"
          _ -> fail "Unable to parse SemVer2Tuple"

instance ToJSON SemVer2Tuple where
  toJSON (SemVer2Tuple maj min) =
    String $ T.pack (show maj ++ "." ++ show min)

instance FromHttpApiData SemVer2Tuple where
  parseQueryParam versionT =
    let parts = splitOn "." . T.unpack $ versionT
     in case parts of
          [majorS, minorS] ->
            case (readMaybe majorS, readMaybe minorS) of
              (Just major, Just minor) -> Right $ SemVer2Tuple major minor
              _ -> Left "Invalid SemVer2Tuple format"
          [majorS] ->
            case readMaybe majorS of
              Just major -> Right $ SemVer2Tuple major 0
              _ -> Left "Invalid SemVer2Tuple format"
          _ -> Left "Unable to parse SemVer2Tuple"

instance ToHttpApiData SemVer2Tuple where
  toUrlPiece (SemVer2Tuple major minor) = toUrlPiece major <> "," <> toUrlPiece minor
