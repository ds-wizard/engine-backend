module Model.Event.Expert.DeleteExpertEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import LensesConfig
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
  equalsUuid e ch = ch ^. uuid == e ^. dexpChapterUuid

instance SameUuid DeleteExpertEvent Question where
  equalsUuid e q = q ^. uuid == e ^. dexpQuestionUuid

instance SameUuid DeleteExpertEvent Expert where
  equalsUuid e exp = exp ^. uuid == e ^. dexpExpertUuid
