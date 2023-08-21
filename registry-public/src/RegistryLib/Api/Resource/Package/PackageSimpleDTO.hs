module RegistryLib.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO

data PackageSimpleDTO = PackageSimpleDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , description :: String
  , organization :: OrganizationSimpleDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
