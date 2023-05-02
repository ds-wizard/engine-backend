module Wizard.Api.Resource.Questionnaire.QuestionnaireJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON QuestionnaireDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDTO where
  toJSON = genericToJSON jsonOptions
