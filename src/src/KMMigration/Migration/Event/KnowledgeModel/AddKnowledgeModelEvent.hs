module KMMigration.Migration.Event.KnowledgeModel.AddKnowledgeModelEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { _akmUuid :: UUID
  , _akmKmUuid :: UUID
  , _akmName :: String
  }

makeLenses ''AddKnowledgeModelEvent

instance SameUuid AddKnowledgeModelEvent KnowledgeModel where
  equalsUuid e km = km ^. kmUuid == e ^. akmKmUuid
