module Model.Event.Reference.EditReferenceEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import LensesConfig
import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data EditReferenceEvent = EditReferenceEvent
  { _erefUuid :: UUID
  , _erefKmUuid :: UUID
  , _erefChapterUuid :: UUID
  , _erefQuestionUuid :: UUID
  , _erefReferenceUuid :: UUID
  , _erefChapter :: Maybe String
  } deriving (Show, Eq, Generic)

makeLenses ''EditReferenceEvent

instance SameUuid EditReferenceEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. erefChapterUuid

instance SameUuid EditReferenceEvent Question where
  equalsUuid e q = q ^. uuid == e ^. erefQuestionUuid

instance SameUuid EditReferenceEvent Reference where
  equalsUuid e ref = ref ^. uuid == e ^. erefReferenceUuid
