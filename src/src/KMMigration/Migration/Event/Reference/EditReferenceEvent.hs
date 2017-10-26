module KMMigration.Migration.Event.Reference.EditReferenceEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data EditReferenceEvent = EditReferenceEvent
  { _erefUuid :: UUID
  , _erefKmUuid :: UUID
  , _erefChapterUuid :: UUID
  , _erefQuestionUuid :: UUID
  , _erefReferenceUuid :: UUID
  , _erefChapter :: Maybe String
  }

makeLenses ''EditReferenceEvent

instance SameUuid EditReferenceEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. erefChapterUuid

instance SameUuid EditReferenceEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. erefQuestionUuid

instance SameUuid EditReferenceEvent Reference where
  equalsUuid e ref = ref ^. refUuid == e ^. erefReferenceUuid
