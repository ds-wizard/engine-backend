module WizardLib.KnowledgeModel.Model.Package.PackageWithEvents where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.Package

data PackageWithEvents = PackageWithEvents
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
  , events :: [Event]
  , nonEditable :: Bool
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
