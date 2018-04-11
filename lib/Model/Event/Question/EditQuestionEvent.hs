module Model.Event.Question.EditQuestionEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import LensesConfig
import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data EditQuestionEvent = EditQuestionEvent
  { _eqUuid :: UUID
  , _eqKmUuid :: UUID
  , _eqChapterUuid :: UUID
  , _eqQuestionUuid :: UUID
  , _eqShortQuestionUuid :: Maybe (Maybe String)
  , _eqType :: Maybe QuestionType
  , _eqTitle :: Maybe String
  , _eqText :: Maybe String
  , _eqAnswerIds :: Maybe [UUID]
  , _eqExpertIds :: Maybe [UUID]
  , _eqReferenceIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

makeLenses ''EditQuestionEvent

instance SameUuid EditQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. eqChapterUuid

instance SameUuid EditQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. eqQuestionUuid
