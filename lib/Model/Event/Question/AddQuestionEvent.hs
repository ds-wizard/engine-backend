module Model.Event.Question.AddQuestionEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import LensesConfig
import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data AddQuestionEvent = AddQuestionEvent
  { _aqUuid :: UUID
  , _aqKmUuid :: UUID
  , _aqChapterUuid :: UUID
  , _aqQuestionUuid :: UUID
  , _aqShortQuestionUuid :: Maybe String
  , _aqType :: QuestionType
  , _aqTitle :: String
  , _aqText :: String
  } deriving (Show, Eq, Generic)

makeLenses ''AddQuestionEvent

instance SameUuid AddQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. aqChapterUuid

instance SameUuid AddQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. aqQuestionUuid
