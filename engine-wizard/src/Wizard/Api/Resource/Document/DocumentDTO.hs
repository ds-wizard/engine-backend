module Wizard.Api.Resource.Document.DocumentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.QuestionnaireSimple

data DocumentDTO = DocumentDTO
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , questionnaire :: Maybe QuestionnaireSimple
  , questionnaireEventUuid :: Maybe U.UUID
  , documentTemplate :: DocumentTemplateSimpleDTO
  , formatUuid :: U.UUID
  , fileName :: Maybe String
  , contentType :: Maybe String
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , submissions :: [SubmissionDTO]
  , creatorUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
