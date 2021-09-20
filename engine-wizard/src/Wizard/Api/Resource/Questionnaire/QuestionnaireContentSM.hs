module Wizard.Api.Resource.Questionnaire.QuestionnaireContentSM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireContentDTO where
  declareNamedSchema = simpleToSchema (toContentDTO questionnaire1Ctn (questionnaire1Ctn ^. commentThreadsMap) [] [])
