module KMMigration.Migration.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data DeleteFollowUpQuestionEvent = DeleteFollowUpQuestionEvent
  { _dfuqUuid :: UUID
  , _dfuqKmUuid :: UUID
  , _dfuqChapterUuid :: UUID
  , _dfuqAnswerUuid :: UUID
  , _dfuqQuestionUuid :: UUID
  }

makeLenses ''DeleteFollowUpQuestionEvent

instance SameUuid DeleteFollowUpQuestionEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dfuqChapterUuid

instance SameUuid DeleteFollowUpQuestionEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. dfuqQuestionUuid

instance SameUuid DeleteFollowUpQuestionEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. dfuqAnswerUuid
