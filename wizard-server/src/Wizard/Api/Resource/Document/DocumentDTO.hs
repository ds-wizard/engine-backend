module Wizard.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.QuestionnaireSimple
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO

data DocumentDTO = DocumentDTO
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , questionnaire :: Maybe QuestionnaireSimple
  , questionnaireEventUuid :: Maybe U.UUID
  , questionnaireVersion :: Maybe String
  , documentTemplateId :: String
  , documentTemplateName :: String
  , format :: Maybe DocumentTemplateFormatDTO
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , submissions :: [SubmissionDTO]
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
