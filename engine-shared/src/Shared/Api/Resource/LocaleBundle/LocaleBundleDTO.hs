module Shared.Api.Resource.LocaleBundle.LocaleBundleDTO where

import Data.Time
import GHC.Generics

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
