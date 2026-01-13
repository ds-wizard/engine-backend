module Shared.Locale.Model.Locale.Locale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Coordinate.Model.Coordinate.Coordinate

data Locale = Locale
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , defaultLocale :: Bool
  , license :: String
  , readme :: String
  , recommendedAppVersion :: String
  , enabled :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord Locale where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.localeId b.localeId
      <> compare a.version b.version

instance CoordinateFactory Locale where
  createCoordinate locale =
    Coordinate
      { organizationId = locale.organizationId
      , entityId = locale.localeId
      , version = locale.version
      }
