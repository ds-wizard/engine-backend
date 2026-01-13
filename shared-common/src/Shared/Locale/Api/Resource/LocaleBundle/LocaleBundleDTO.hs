module Shared.Locale.Api.Resource.LocaleBundle.LocaleBundleDTO where

import Data.Time
import GHC.Generics

import Shared.Coordinate.Model.Coordinate.Coordinate

data LocaleBundleDTO = LocaleBundleDTO
  { lId :: String
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , license :: String
  , readme :: String
  , recommendedAppVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory LocaleBundleDTO where
  createCoordinate locale =
    Coordinate
      { organizationId = locale.organizationId
      , entityId = locale.localeId
      , version = locale.version
      }
