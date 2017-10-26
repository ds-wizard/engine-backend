module KMMigration.Migration.Event.KnowledgeModel.EditKnowledgeModelEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { _ekmUuid :: UUID
  , _ekmKmUuid :: UUID
  , _ekmName :: Maybe String
  , _ekmChapterIds :: Maybe [UUID]
  }

makeLenses ''EditKnowledgeModelEvent

instance SameUuid EditKnowledgeModelEvent KnowledgeModel where
  equalsUuid e km = km ^. kmUuid == e ^. ekmKmUuid
