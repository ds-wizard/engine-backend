module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO

data QuestionnaireVersionDTO =
  QuestionnaireVersionDTO
    { _questionnaireVersionDTOUuid :: U.UUID
    , _questionnaireVersionDTOName :: String
    , _questionnaireVersionDTODescription :: Maybe String
    , _questionnaireVersionDTOEventUuid :: U.UUID
    , _questionnaireVersionDTOCreatedBy :: UserSuggestionDTO
    , _questionnaireVersionDTOCreatedAt :: UTCTime
    , _questionnaireVersionDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq QuestionnaireVersionDTO where
  a == b =
    _questionnaireVersionDTOUuid a == _questionnaireVersionDTOUuid b &&
    _questionnaireVersionDTOName a == _questionnaireVersionDTOName b &&
    _questionnaireVersionDTODescription a == _questionnaireVersionDTODescription b &&
    _questionnaireVersionDTOEventUuid a == _questionnaireVersionDTOEventUuid b &&
    _questionnaireVersionDTOCreatedBy a == _questionnaireVersionDTOCreatedBy b
