module Wizard.Api.Resource.Package.PackageDetailDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Package.Package
import Wizard.Model.Package.PackageState
import Wizard.Model.Registry.RegistryOrganization

data PackageDetailDTO = PackageDetailDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: PackagePhase
  , description :: String
  , readme :: String
  , license :: String
  , metamodelVersion :: Int
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , mergeCheckpointPackageId :: Maybe String
  , versions :: [String]
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe RegistryOrganization
  , registryLink :: Maybe String
  , state :: PackageState
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
