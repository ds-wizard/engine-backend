module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()

instance FromJSON QuestionnaireDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailDTO where
  toJSON = genericToJSON jsonOptions
