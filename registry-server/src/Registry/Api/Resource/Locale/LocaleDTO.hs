module Registry.Api.Resource.Locale.LocaleDTO where

import Data.Time
import GHC.Generics

import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO

data LocaleDTO = LocaleDTO
  { lId :: String
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , organization :: Maybe OrganizationSimpleDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
