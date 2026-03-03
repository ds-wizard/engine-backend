module Shared.Coordinate.Database.Mapping.Coordinate.Coordinate where

import Data.String (fromString)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.Coordinate.Util.Coordinate

-- FROM database
instance FromField Coordinate where
  fromField _ (Just bs) = either (error . ("Parse error: " ++)) pure (parseCoordinateBS bs)
  fromField f Nothing = returnError UnexpectedNull f "NULL Coordinate"

-- TO database
instance ToField Coordinate where
  toField Coordinate {..} =
    let txt = organizationId ++ ":" ++ entityId ++ ":" ++ version
     in Escape (fromString txt)
