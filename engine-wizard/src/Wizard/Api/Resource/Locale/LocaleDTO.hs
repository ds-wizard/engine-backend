module Wizard.Api.Resource.Locale.LocaleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Model.Locale.LocaleState

data LocaleDTO = LocaleDTO
  { lId :: String
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , defaultLocale :: Bool
  , enabled :: Bool
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe OrganizationSimpleDTO
  , state :: LocaleState
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
