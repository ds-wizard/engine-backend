module Registry.Api.Resource.Locale.LocaleDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple

data LocaleDetailDTO = LocaleDetailDTO
  { uuid :: U.UUID
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
  , organization :: OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
