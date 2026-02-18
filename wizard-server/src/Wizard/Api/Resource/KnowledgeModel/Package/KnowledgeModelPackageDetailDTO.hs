module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Api.Resource.Version.VersionDTO
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Model.Registry.RegistryOrganization

data KnowledgeModelPackageDetailDTO = KnowledgeModelPackageDetailDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , description :: String
  , readme :: String
  , license :: String
  , metamodelVersion :: Int
  , previousPackageUuid :: Maybe U.UUID
  , forkOfPackageId :: Maybe Coordinate
  , mergeCheckpointPackageId :: Maybe Coordinate
  , nonEditable :: Bool
  , public :: Bool
  , versions :: [VersionDTO]
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe RegistryOrganization
  , registryLink :: Maybe String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
