module WizardLib.KnowledgeModel.Api.Resource.Package.PackageDTO where

import Data.Time
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.Package

data PackageDTO = PackageDTO
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
  , nonEditable :: Bool
  , events :: [Event]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
