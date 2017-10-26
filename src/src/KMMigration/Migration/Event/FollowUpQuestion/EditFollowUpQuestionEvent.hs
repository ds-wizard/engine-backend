module KMMigration.Migration.Event.FollowUpQuestion.EditFollowUpQuestionEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data EditFollowUpQuestionEvent = EditFollowUpQuestionEvent
  { _efuqUuid :: UUID
  , _efuqKmUuid :: UUID
  , _efuqChapterUuid :: UUID
  , _efuqAnswerUuid :: UUID
  , _efuqQuestionUuid :: UUID
  , _efuqType :: Maybe String
  , _efuqTitle :: Maybe String
  , _efuqText :: Maybe String
  , _efuqAnswerIds :: Maybe [UUID]
  , _efuqExpertIds :: Maybe [UUID]
  , _efuqReferenceIds :: Maybe [UUID]
  }

makeLenses ''EditFollowUpQuestionEvent

instance SameUuid EditFollowUpQuestionEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. efuqChapterUuid

instance SameUuid EditFollowUpQuestionEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. efuqQuestionUuid

instance SameUuid EditFollowUpQuestionEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. efuqAnswerUuid
