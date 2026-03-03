module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Coordinate.Model.Coordinate.Coordinate

data KnowledgeModelPackagePhase
  = ReleasedKnowledgeModelPackagePhase
  | DeprecatedKnowledgeModelPackagePhase
  deriving (Show, Eq, Generic, Read)

data KnowledgeModelPackage = KnowledgeModelPackage
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , previousPackageUuid :: Maybe U.UUID
  , forkOfPackageId :: Maybe Coordinate
  , mergeCheckpointPackageId :: Maybe Coordinate -- TODO fix it
  , nonEditable :: Bool
  , public :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord KnowledgeModelPackage where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.kmId b.kmId
      <> compare a.version b.version

instance CoordinateFactory KnowledgeModelPackage where
  createCoordinate p =
    Coordinate
      { organizationId = p.organizationId
      , entityId = p.kmId
      , version = p.version
      }
