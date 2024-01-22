module Wizard.Api.Resource.Locale.LocaleDetailDTO where

import Data.Time
import GHC.Generics

import Wizard.Model.Registry.RegistryOrganization

data LocaleDetailDTO = LocaleDetailDTO
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
  , versions :: [String]
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe RegistryOrganization
  , registryLink :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
