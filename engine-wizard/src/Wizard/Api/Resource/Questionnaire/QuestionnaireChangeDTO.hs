module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl

data QuestionnaireChangeDTO =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName :: String
    , _questionnaireChangeDTOVisibility :: QuestionnaireVisibility
    , _questionnaireChangeDTOSharing :: QuestionnaireSharing
    , _questionnaireChangeDTOPermissions :: [QuestionnairePermRecord]
    , _questionnaireChangeDTOTemplateId :: Maybe String
    , _questionnaireChangeDTOFormatUuid :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)
