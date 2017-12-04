module Model.Event.FollowUpQuestion.AddFollowUpQuestionEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data AddFollowUpQuestionEvent = AddFollowUpQuestionEvent
  { _afuqUuid :: UUID
  , _afuqKmUuid :: UUID
  , _afuqChapterUuid :: UUID
  , _afuqAnswerUuid :: UUID
  , _afuqQuestionUuid :: UUID
  , _afuqShortQuestionUuid :: Maybe String
  , _afuqType :: String
  , _afuqTitle :: String
  , _afuqText :: String
  } deriving (Show, Eq, Generic)

makeLenses ''AddFollowUpQuestionEvent

instance SameUuid AddFollowUpQuestionEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. afuqChapterUuid

instance SameUuid AddFollowUpQuestionEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. afuqQuestionUuid

instance SameUuid AddFollowUpQuestionEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. afuqAnswerUuid
