module KMMigration.Migration.Event.Expert.DeleteExpertEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data DeleteExpertEvent = DeleteExpertEvent
  { _dexpUuid :: UUID
  , _dexpKmUuid :: UUID
  , _dexpChapterUuid :: UUID
  , _dexpQuestionUuid :: UUID
  , _dexpExpertUuid :: UUID
  }

makeLenses ''DeleteExpertEvent

instance SameUuid DeleteExpertEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dexpChapterUuid

instance SameUuid DeleteExpertEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. dexpQuestionUuid

instance SameUuid DeleteExpertEvent Expert where
  equalsUuid e exp = exp ^. expUuid == e ^. dexpExpertUuid
