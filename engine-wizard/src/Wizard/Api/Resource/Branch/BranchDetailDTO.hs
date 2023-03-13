module Wizard.Api.Resource.Branch.BranchDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Branch.BranchState

data BranchDetailDTO = BranchDetailDTO
  { uuid :: U.UUID
  , name :: String
  , kmId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , state :: BranchState
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , forkOfPackage :: Maybe PackageSimpleDTO
  , createdBy :: Maybe U.UUID
  , events :: [Event]
  , knowledgeModel :: KnowledgeModel
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq BranchDetailDTO where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.kmId == b.kmId
      && a.version == b.version
      && a.description == b.description
      && a.readme == b.readme
      && a.license == b.license
      && a.state == b.state
      && a.previousPackageId == b.previousPackageId
      && a.forkOfPackageId == b.forkOfPackageId
      && a.forkOfPackage == b.forkOfPackage
      && a.createdBy == b.createdBy
      && a.events == b.events
      && a.knowledgeModel == b.knowledgeModel
