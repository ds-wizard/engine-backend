module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview

instance FromJSON QuestionnaireDetailPreview where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailPreview where
  toJSON = genericToJSON jsonOptions
