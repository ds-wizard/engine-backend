module KMMigration.Migration.Event.Answer.DeleteAnswerEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data DeleteAnswerEvent = DeleteAnswerEvent
  { _dansUuid :: UUID
  , _dansKmUuid :: UUID
  , _dansChapterUuid :: UUID
  , _dansQuestionUuid :: UUID
  , _dansAnswerUuid :: UUID
  }

makeLenses ''DeleteAnswerEvent

instance SameUuid DeleteAnswerEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dansChapterUuid

instance SameUuid DeleteAnswerEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. dansQuestionUuid

instance SameUuid DeleteAnswerEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. dansAnswerUuid
