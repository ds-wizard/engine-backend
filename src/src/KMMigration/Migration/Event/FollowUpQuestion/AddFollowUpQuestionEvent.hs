module KMMigration.Migration.Event.FollowUpQuestion.AddFollowUpQuestionEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data AddFollowUpQuestionEvent = AddFollowUpQuestionEvent
  { _afuqUuid :: UUID
  , _afuqKmUuid :: UUID
  , _afuqChapterUuid :: UUID
  , _afuqAnswerUuid :: UUID
  , _afuqQuestionUuid :: UUID
  , _afuqType :: String
  , _afuqTitle :: String
  , _afuqText :: String
  }

makeLenses ''AddFollowUpQuestionEvent

instance SameUuid AddFollowUpQuestionEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. afuqChapterUuid

instance SameUuid AddFollowUpQuestionEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. afuqQuestionUuid

instance SameUuid AddFollowUpQuestionEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. afuqAnswerUuid
