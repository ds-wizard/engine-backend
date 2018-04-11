module Model.Event.Reference.DeleteReferenceEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import LensesConfig
import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data DeleteReferenceEvent = DeleteReferenceEvent
  { _drefUuid :: UUID
  , _drefKmUuid :: UUID
  , _drefChapterUuid :: UUID
  , _drefQuestionUuid :: UUID
  , _drefReferenceUuid :: UUID
  } deriving (Show, Eq, Generic)

makeLenses ''DeleteReferenceEvent

instance SameUuid DeleteReferenceEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. drefChapterUuid

instance SameUuid DeleteReferenceEvent Question where
  equalsUuid e q = q ^. uuid == e ^. drefQuestionUuid

instance SameUuid DeleteReferenceEvent Reference where
  equalsUuid e ref = ref ^. uuid == e ^. drefReferenceUuid
