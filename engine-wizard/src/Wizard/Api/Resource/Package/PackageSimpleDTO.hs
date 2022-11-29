module Wizard.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import Wizard.Model.Package.PackageState
import Wizard.Model.Registry.RegistryOrganization

data PackageSimpleDTO = PackageSimpleDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , remoteLatestVersion :: Maybe String
  , description :: String
  , state :: PackageState
  , organization :: Maybe RegistryOrganization
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
