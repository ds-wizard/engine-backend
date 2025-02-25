module Wizard.Api.Resource.Questionnaire.QuestionnaireContentSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireContentDTO where
  declareNamedSchema = toSwagger (toContentDTO questionnaire1Ctn M.empty [] [])
