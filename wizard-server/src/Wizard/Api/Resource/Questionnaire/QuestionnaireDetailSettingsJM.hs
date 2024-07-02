module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()

instance FromJSON QuestionnaireDetailSettings where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailSettings where
  toJSON = genericToJSON jsonOptions
