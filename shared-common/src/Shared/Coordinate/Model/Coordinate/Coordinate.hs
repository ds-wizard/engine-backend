module Shared.Coordinate.Model.Coordinate.Coordinate where

import GHC.Generics

data Coordinate = Coordinate
  { organizationId :: String
  , entityId :: String
  , version :: String
  }
  deriving (Eq, Generic)

instance Show Coordinate where
  show (Coordinate {..}) = organizationId ++ ":" ++ entityId ++ ":" ++ version

class CoordinateFactory entity where
  createCoordinate :: entity -> Coordinate
