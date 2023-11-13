module Shared.Locale.Model.Locale.Locale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Locale = Locale
  { lId :: String
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
