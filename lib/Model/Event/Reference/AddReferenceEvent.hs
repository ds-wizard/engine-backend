module Model.Event.Reference.AddReferenceEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import LensesConfig
import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data AddReferenceEvent = AddReferenceEvent
  { _arefUuid :: UUID
  , _arefKmUuid :: UUID
  , _arefChapterUuid :: UUID
  , _arefQuestionUuid :: UUID
  , _arefReferenceUuid :: UUID
  , _arefChapter :: String
  } deriving (Show, Eq, Generic)

makeLenses ''AddReferenceEvent

instance SameUuid AddReferenceEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. arefChapterUuid

instance SameUuid AddReferenceEvent Question where
  equalsUuid e q = q ^. uuid == e ^. arefQuestionUuid

instance SameUuid AddReferenceEvent Reference where
  equalsUuid e ref = ref ^. uuid == e ^. arefReferenceUuid
