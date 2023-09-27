module WizardLib.KnowledgeModel.Model.Package.PackageWithEventsRaw where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Package.Package

data PackageWithEventsRaw = PackageWithEventsRaw
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: PackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , mergeCheckpointPackageId :: Maybe String
  , events :: Value
  , nonEditable :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
