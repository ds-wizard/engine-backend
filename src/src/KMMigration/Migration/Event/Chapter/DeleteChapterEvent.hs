module KMMigration.Migration.Event.Chapter.DeleteChapterEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data DeleteChapterEvent = DeleteChapterEvent
  { _dchUuid :: UUID
  , _dchKmUuid :: UUID
  , _dchChapterUuid :: UUID
  }

makeLenses ''DeleteChapterEvent

instance SameUuid DeleteChapterEvent KnowledgeModel where
  equalsUuid e km = km ^. kmUuid == e ^. dchKmUuid

instance SameUuid DeleteChapterEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dchChapterUuid
