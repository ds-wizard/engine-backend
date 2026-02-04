module Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage where

import Data.Time
import GHC.Generics

import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelBundlePackage = KnowledgeModelBundlePackage
  { pId :: Coordinate
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , previousPackageId :: Maybe Coordinate
  , forkOfPackageId :: Maybe Coordinate
  , mergeCheckpointPackageId :: Maybe Coordinate
  , events :: [KnowledgeModelEvent]
  , nonEditable :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory KnowledgeModelBundlePackage where
  createCoordinate dt =
    Coordinate
      { organizationId = dt.organizationId
      , entityId = dt.kmId
      , version = dt.version
      }
