module KMMigration.Migration.Event.Question.AddQuestionEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data AddQuestionEvent = AddQuestionEvent
  { _aqUuid :: UUID
  , _aqKmUuid :: UUID
  , _aqChapterUuid :: UUID
  , _aqQuestionUuid :: UUID
  , _aqType :: String
  , _aqTitle :: String
  , _aqText :: String
  }

makeLenses ''AddQuestionEvent

instance SameUuid AddQuestionEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. aqChapterUuid

instance SameUuid AddQuestionEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. aqQuestionUuid
