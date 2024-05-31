module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON QuestionnaireDetailPreview where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailPreview where
  toJSON = genericToJSON jsonOptions
