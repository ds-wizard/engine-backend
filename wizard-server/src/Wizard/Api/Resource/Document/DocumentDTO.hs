module Wizard.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Model.Submission.SubmissionList

data DocumentDTO = DocumentDTO
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , questionnaire :: Maybe QuestionnaireSimple
  , questionnaireEventUuid :: Maybe U.UUID
  , questionnaireVersion :: Maybe String
  , documentTemplateId :: String
  , documentTemplateName :: String
  , format :: Maybe DocumentTemplateFormatSimple
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , submissions :: [SubmissionList]
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
