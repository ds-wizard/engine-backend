module KMMigration.Migration.Event.Answer.AddAnswerEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data AddAnswerEvent = AddAnswerEvent
  { _aansUuid :: UUID
  , _aansKmUuid :: UUID
  , _aansChapterUuid :: UUID
  , _aansQuestionUuid :: UUID
  , _aansAnswerUuid :: UUID
  , _aansLabel :: String
  , _aansAdvice :: Maybe String
  }

makeLenses ''AddAnswerEvent

instance SameUuid AddAnswerEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. aansChapterUuid

instance SameUuid AddAnswerEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. aansQuestionUuid

instance SameUuid AddAnswerEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. aansAnswerUuid
