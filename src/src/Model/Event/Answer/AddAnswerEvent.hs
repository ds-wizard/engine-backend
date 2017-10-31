module Model.Event.Answer.AddAnswerEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import Model.KnowledgeModel.KnowledgeModel

data AddAnswerEvent = AddAnswerEvent
  { _aansUuid :: UUID
  , _aansKmUuid :: UUID
  , _aansChapterUuid :: UUID
  , _aansQuestionUuid :: UUID
  , _aansAnswerUuid :: UUID
  , _aansLabel :: String
  , _aansAdvice :: Maybe String
  } deriving (Show, Eq, Generic)

makeLenses ''AddAnswerEvent

instance SameUuid AddAnswerEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. aansChapterUuid

instance SameUuid AddAnswerEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. aansQuestionUuid

instance SameUuid AddAnswerEvent Answer where
  equalsUuid e ans = ans ^. ansUuid == e ^. aansAnswerUuid
