module Model.KnowledgeModelContainer.KnowledgeModelContainer where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelContainer = KnowledgeModelContainer
  { _kmcKmcUuid :: UUID
  , _kmcName :: String
  , _kmcArtifactId :: String
  , _kmcParentPackageId :: Maybe String
  } deriving (Show, Eq, Generic)

data KnowledgeModelContainerWithEvents = KnowledgeModelContainerWithEvents
  { _kmcweKmContainerUuid :: UUID
  , _kmcweName :: String
  , _kmcweArtifactId :: String
  , _kmcweParentPackageId :: Maybe String
  , _kmcweEvents :: [Event]
  } deriving (Generic)

data KnowledgeModelContainerWithKM = KnowledgeModelContainerWithKM
  { _kmcwkmKmContainerUuid :: UUID
  , _kmcwkmName :: String
  , _kmcwkmArtifactId :: String
  , _kmcwkmParentPackageId :: Maybe String
  , _kmcwkmKM :: Maybe KnowledgeModel
  } deriving (Show, Eq, Generic)

makeLenses ''KnowledgeModelContainer

makeLenses ''KnowledgeModelContainerWithEvents

makeLenses ''KnowledgeModelContainerWithKM
