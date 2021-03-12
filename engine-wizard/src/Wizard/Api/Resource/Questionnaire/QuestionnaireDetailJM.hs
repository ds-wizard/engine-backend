module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.Template.TemplateJM ()
import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()

instance FromJSON QuestionnaireDetailDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireDetailDTO where
  toJSON = genericToJSON simpleOptions
