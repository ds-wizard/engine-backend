module Model.KnowledgeModelContainer.KnowledgeModelContainer where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelContainer = KnowledgeModelContainer
  { _kmcKmContainerUuid :: UUID
  , _kmcName :: String
  , _kmcShortName :: String
  , _kmcParentPackageName :: String
  , _kmcParentPackageVersion :: String
  } deriving (Show, Eq, Generic)

data KnowledgeModelContainerWithEvents = KnowledgeModelContainerWithEvents
  { _kmcweKmContainerUuid :: UUID
  , _kmcweName :: String
  , _kmcweShortName :: String
  , _kmcweParentPackageName :: String
  , _kmcweParentPackageVersion :: String
  , _kmcweEvents :: [Event]
  } deriving (Generic)

data KnowledgeModelContainerWithKM = KnowledgeModelContainerWithKM
  { _kmcwkmKmContainerUuid :: UUID
  , _kmcwkmName :: String
  , _kmcwkmShortName :: String
  , _kmcwkmParentPackageName :: String
  , _kmcwkmParentPackageVersion :: String
  , _kmcwkmKM :: KnowledgeModel
  } deriving (Show, Eq, Generic)

makeLenses ''KnowledgeModelContainer

makeLenses ''KnowledgeModelContainerWithEvents

makeLenses ''KnowledgeModelContainerWithKM
