module Model.Event.Expert.DeleteExpertEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data DeleteExpertEvent = DeleteExpertEvent
  { _dexpUuid :: UUID
  , _dexpKmUuid :: UUID
  , _dexpChapterUuid :: UUID
  , _dexpQuestionUuid :: UUID
  , _dexpExpertUuid :: UUID
  } deriving (Show, Eq, Generic)

makeLenses ''DeleteExpertEvent

instance SameUuid DeleteExpertEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dexpChapterUuid

instance SameUuid DeleteExpertEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. dexpQuestionUuid

instance SameUuid DeleteExpertEvent Expert where
  equalsUuid e exp = exp ^. expUuid == e ^. dexpExpertUuid
