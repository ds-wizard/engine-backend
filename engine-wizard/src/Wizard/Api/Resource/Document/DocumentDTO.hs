module Wizard.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateSimpleJM ()
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.QuestionnaireSimple

data DocumentDTO =
  DocumentDTO
    { _documentDTOUuid :: U.UUID
    , _documentDTOName :: String
    , _documentDTOState :: DocumentState
    , _documentDTOQuestionnaire :: Maybe QuestionnaireSimple
    , _documentDTOQuestionnaireEventUuid :: Maybe U.UUID
    , _documentDTOTemplate :: TemplateSimpleDTO
    , _documentDTOFormatUuid :: U.UUID
    , _documentDTOFileName :: Maybe String
    , _documentDTOContentType :: Maybe String
    , _documentDTOFileSize :: Maybe Int64
    , _documentDTOSubmissions :: [SubmissionDTO]
    , _documentDTOCreatorUuid :: Maybe U.UUID
    , _documentDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
