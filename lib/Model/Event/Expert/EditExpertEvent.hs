module Model.Event.Expert.EditExpertEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data EditExpertEvent = EditExpertEvent
  { _eexpUuid :: UUID
  , _eexpKmUuid :: UUID
  , _eexpChapterUuid :: UUID
  , _eexpQuestionUuid :: UUID
  , _eexpExpertUuid :: UUID
  , _eexpName :: Maybe String
  , _eexpEmail :: Maybe String
  } deriving (Show, Eq, Generic)

makeLenses ''EditExpertEvent

instance SameUuid EditExpertEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. eexpChapterUuid

instance SameUuid EditExpertEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. eexpQuestionUuid

instance SameUuid EditExpertEvent Expert where
  equalsUuid e exp = exp ^. expUuid == e ^. eexpExpertUuid
