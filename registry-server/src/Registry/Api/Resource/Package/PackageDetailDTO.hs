module Registry.Api.Resource.Package.PackageDetailDTO where

import Data.Time
import GHC.Generics

import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.KnowledgeModel.Model.Package.Package

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
  , organization :: OrganizationSimpleDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
