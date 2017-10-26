module KMMigration.Migration.Event.Reference.AddReferenceEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data AddReferenceEvent = AddReferenceEvent
  { _arefUuid :: UUID
  , _arefKmUuid :: UUID
  , _arefChapterUuid :: UUID
  , _arefQuestionUuid :: UUID
  , _arefReferenceUuid :: UUID
  , _arefChapter :: String
  }

makeLenses ''AddReferenceEvent

instance SameUuid AddReferenceEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. arefChapterUuid

instance SameUuid AddReferenceEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. arefQuestionUuid

instance SameUuid AddReferenceEvent Reference where
  equalsUuid e ref = ref ^. refUuid == e ^. arefReferenceUuid
