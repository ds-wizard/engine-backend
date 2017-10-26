module KMMigration.Migration.Event.Expert.AddExpertEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data AddExpertEvent = AddExpertEvent
  { _aexpUuid :: UUID
  , _aexpKmUuid :: UUID
  , _aexpChapterUuid :: UUID
  , _aexpQuestionUuid :: UUID
  , _aexpExpertUuid :: UUID
  , _aexpName :: String
  , _aexpEmail :: String
  }

makeLenses ''AddExpertEvent

instance SameUuid AddExpertEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. aexpChapterUuid

instance SameUuid AddExpertEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. aexpQuestionUuid

instance SameUuid AddExpertEvent Expert where
  equalsUuid e exp = exp ^. expUuid == e ^. aexpExpertUuid
