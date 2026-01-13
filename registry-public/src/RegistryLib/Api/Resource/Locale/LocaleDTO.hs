module RegistryLib.Api.Resource.Locale.LocaleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple

data LocaleDTO = LocaleDTO
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , organization :: Maybe OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
