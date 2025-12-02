module Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO where

import Data.Time
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageDetailDTO = KnowledgeModelPackageDetailDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , description :: String
  , readme :: String
  , license :: String
  , metamodelVersion :: Int
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , mergeCheckpointPackageId :: Maybe String
  , versions :: [String]
  , organization :: OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
