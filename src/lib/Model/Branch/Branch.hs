module Model.Branch.Branch where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data Branch = Branch
  { _bUuid :: UUID
  , _bName :: String
  , _bArtifactId :: String
  , _bParentPackageId :: Maybe String
  , _bLastAppliedParentPackageId :: Maybe String
  , _bLastMergeCheckpointPackageId :: Maybe String
  } deriving (Show, Eq, Generic)

data BranchWithEvents = BranchWithEvents
  { _bweUuid :: UUID
  , _bweName :: String
  , _bweArtifactId :: String
  , _bweParentPackageId :: Maybe String
  , _bweLastAppliedParentPackageId :: Maybe String
  , _bweLastMergeCheckpointPackageId :: Maybe String
  , _bweEvents :: [Event]
  } deriving (Generic)

data BranchWithKM = BranchWithKM
  { _bwkmUuid :: UUID
  , _bwkmName :: String
  , _bwkmArtifactId :: String
  , _bwkmParentPackageId :: Maybe String
  , _bwkmLastAppliedParentPackageId :: Maybe String
  , _bwkmLastMergeCheckpointPackageId :: Maybe String
  , _bwkmKM :: Maybe KnowledgeModel
  } deriving (Show, Eq, Generic)

makeLenses ''Branch

makeLenses ''BranchWithEvents

makeLenses ''BranchWithKM
