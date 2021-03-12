module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireVersionChangeDTO =
  QuestionnaireVersionChangeDTO
    { _questionnaireVersionChangeDTOName :: String
    , _questionnaireVersionChangeDTODescription :: Maybe String
    , _questionnaireVersionChangeDTOEventUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
