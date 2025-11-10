module Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw where

import Data.Aeson
import Data.Time
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageRaw = KnowledgeModelPackageRaw
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
  , mergeCheckpointPackageId :: Maybe String
  , events :: Value
  , nonEditable :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
