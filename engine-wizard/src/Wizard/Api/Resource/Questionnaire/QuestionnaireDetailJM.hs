module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()

instance FromJSON QuestionnaireDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailDTO where
  toJSON = genericToJSON jsonOptions
