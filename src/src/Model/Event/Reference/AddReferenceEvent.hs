module Model.Event.Reference.AddReferenceEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Event.Common
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
  equalsUuid e ch = ch ^. chUuid == e ^. arefChapterUuid

instance SameUuid AddReferenceEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. arefQuestionUuid

instance SameUuid AddReferenceEvent Reference where
  equalsUuid e ref = ref ^. refUuid == e ^. arefReferenceUuid
