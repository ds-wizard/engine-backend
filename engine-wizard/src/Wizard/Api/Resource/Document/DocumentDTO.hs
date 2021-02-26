module Wizard.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateSimpleJM ()
import Wizard.Model.Document.Document

data DocumentDTO =
  DocumentDTO
    { _documentDTOUuid :: U.UUID
    , _documentDTOName :: String
    , _documentDTOState :: DocumentState
    , _documentDTOQuestionnaire :: Maybe QuestionnaireDTO
    , _documentDTOQuestionnaireEventUuid :: Maybe U.UUID
    , _documentDTOTemplate :: TemplateSimpleDTO
    , _documentDTOFormatUuid :: U.UUID
    , _documentDTOCreatorUuid :: Maybe U.UUID
    , _documentDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
