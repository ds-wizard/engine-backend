module Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw where

import Data.Aeson
import Data.Time
import GHC.Generics

import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageRaw = KnowledgeModelPackageRaw
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
  , events :: Value
  , nonEditable :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
