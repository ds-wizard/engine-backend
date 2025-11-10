module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings

instance FromJSON QuestionnaireDetailSettings where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailSettings where
  toJSON = genericToJSON jsonOptions
