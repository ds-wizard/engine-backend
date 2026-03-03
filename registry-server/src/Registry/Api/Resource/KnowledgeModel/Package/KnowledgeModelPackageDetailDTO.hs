module Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

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
  , versions :: [String]
  , organization :: OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
