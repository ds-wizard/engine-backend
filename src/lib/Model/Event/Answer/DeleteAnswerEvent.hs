module Model.Event.Answer.DeleteAnswerEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Event.Common
import Model.KnowledgeModel.KnowledgeModel

data DeleteAnswerEvent = DeleteAnswerEvent
  { _dansUuid :: UUID
  , _dansKmUuid :: UUID
  , _dansChapterUuid :: UUID
  , _dansQuestionUuid :: UUID
  , _dansAnswerUuid :: UUID
  } deriving (Show, Eq, Generic)

makeLenses ''DeleteAnswerEvent

instance SameUuid DeleteAnswerEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dansChapterUuid

instance SameUuid DeleteAnswerEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. dansQuestionUuid

instance SameUuid DeleteAnswerEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. dansAnswerUuid
