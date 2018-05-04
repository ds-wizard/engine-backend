module Model.Questionnaire.Questionnaire where

import Control.Lens (makeLenses)
import Data.Map
import Data.UUID
import GHC.Generics

import Common.Types
import Model.KnowledgeModel.KnowledgeModel

type QuestionnaireReplies = Map String String

data Questionnaire = Questionnaire
  { _questionnaireUuid :: UUID
  , _questionnaireName :: String
  , _questionnairePackageId :: String
  , _questionnaireKnowledgeModel :: KnowledgeModel
  , _questionnaireReplies :: QuestionnaireReplies
  } deriving (Generic, Show, Eq)
