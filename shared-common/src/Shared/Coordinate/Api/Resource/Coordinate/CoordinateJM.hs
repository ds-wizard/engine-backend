module Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM where

import Data.Aeson
import qualified Data.Text as T
import Servant.API

import Shared.Common.Util.String
import Shared.Coordinate.Model.Coordinate.Coordinate

instance FromJSON Coordinate where
  parseJSON = withText "Coordinate" $ \coordinate ->
    let parts = splitOn ":" . T.unpack $ coordinate
     in case parts of
          [organizationId, entityId, version] -> pure $ Coordinate {..}
          _ -> fail $ "Unable to parse Coordinate '" ++ T.unpack coordinate ++ "'"

instance ToJSON Coordinate where
  toJSON (Coordinate {..}) =
    String $ T.pack (organizationId ++ ":" ++ entityId ++ ":" ++ version)

instance FromHttpApiData Coordinate where
  parseQueryParam coordinate =
    let parts = splitOn ":" . T.unpack $ coordinate
     in case parts of
          [organizationId, entityId, version] -> Right $ Coordinate {..}
          _ -> Left . T.pack $ "Unable to parse Coordinate '" ++ T.unpack coordinate ++ "'"

instance ToHttpApiData Coordinate where
  toUrlPiece Coordinate {..} = toUrlPiece organizationId <> ":" <> toUrlPiece entityId <> ":" <> toUrlPiece version
