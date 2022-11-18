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

data DocumentDTO = DocumentDTO
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , questionnaire :: Maybe QuestionnaireSimple
  , questionnaireEventUuid :: Maybe U.UUID
  , template :: TemplateSimpleDTO
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
