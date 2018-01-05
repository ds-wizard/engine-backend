module Model.Event.KnowledgeModel.AddKnowledgeModelEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { _akmUuid :: UUID
  , _akmKmUuid :: UUID
  , _akmName :: String
  } deriving (Show, Eq, Generic)

makeLenses ''AddKnowledgeModelEvent

instance SameUuid AddKnowledgeModelEvent KnowledgeModel where
  equalsUuid e km = km ^. kmUuid == e ^. akmKmUuid
