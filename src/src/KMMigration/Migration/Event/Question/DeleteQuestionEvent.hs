module KMMigration.Migration.Event.Question.DeleteQuestionEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data DeleteQuestionEvent = DeleteQuestionEvent
  { _dqUuid :: UUID
  , _dqKmUuid :: UUID
  , _dqChapterUuid :: UUID
  , _dqQuestionUuid :: UUID
  }

makeLenses ''DeleteQuestionEvent

instance SameUuid DeleteQuestionEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dqChapterUuid

instance SameUuid DeleteQuestionEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. dqQuestionUuid
