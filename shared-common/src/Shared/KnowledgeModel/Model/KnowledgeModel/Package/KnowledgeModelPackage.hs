module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelPackagePhase
  = ReleasedKnowledgeModelPackagePhase
  | DeprecatedKnowledgeModelPackagePhase
  deriving (Show, Eq, Generic, Read)

data KnowledgeModelPackage = KnowledgeModelPackage
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , mergeCheckpointPackageId :: Maybe String -- TODO fix it
  , nonEditable :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord KnowledgeModelPackage where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.kmId b.kmId
      <> compare a.version b.version
