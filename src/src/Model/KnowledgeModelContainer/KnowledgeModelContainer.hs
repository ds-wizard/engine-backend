module Model.KnowledgeModelContainer.KnowledgeModelContainer where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

data KnowledgeModelContainer = KnowledgeModelContainer
  { _kmcKmContainerUuid :: UUID
  , _kmcName :: String
  , _kmcShortName :: String
  , _kmcParentPackageName :: String
  , _kmcParentPackageVersion :: String
  } deriving (Show, Eq, Generic)

makeLenses ''KnowledgeModelContainer
