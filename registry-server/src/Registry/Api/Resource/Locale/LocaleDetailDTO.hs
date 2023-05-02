module Registry.Api.Resource.Locale.LocaleDetailDTO where

import Data.Time
import GHC.Generics

import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO

data LocaleDetailDTO = LocaleDetailDTO
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
  , versions :: [String]
  , organization :: OrganizationSimpleDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
