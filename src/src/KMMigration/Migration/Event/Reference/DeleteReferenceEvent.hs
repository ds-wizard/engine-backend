module KMMigration.Migration.Event.Reference.DeleteReferenceEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data DeleteReferenceEvent = DeleteReferenceEvent
  { _drefUuid :: UUID
  , _drefKmUuid :: UUID
  , _drefChapterUuid :: UUID
  , _drefQuestionUuid :: UUID
  , _drefReferenceUuid :: UUID
  }

makeLenses ''DeleteReferenceEvent

instance SameUuid DeleteReferenceEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. drefChapterUuid

instance SameUuid DeleteReferenceEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. drefQuestionUuid

instance SameUuid DeleteReferenceEvent Reference where
  equalsUuid e ref = ref ^. refUuid == e ^. drefReferenceUuid
