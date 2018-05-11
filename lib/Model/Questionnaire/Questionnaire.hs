module Model.Questionnaire.Questionnaire where

import Data.Map
import Data.UUID
import GHC.Generics

import Model.KnowledgeModel.KnowledgeModel

type QuestionnaireReplies = Map String String

data Questionnaire = Questionnaire
  { _questionnaireUuid :: UUID
  , _questionnaireName :: String
  , _questionnairePackageId :: String
  , _questionnaireKnowledgeModel :: KnowledgeModel
  , _questionnaireReplies :: QuestionnaireReplies
  } deriving (Generic, Show, Eq)
