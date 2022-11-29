module Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Acl.MemberJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Model.Questionnaire.QuestionnaireAcl

instance FromJSON QuestionnairePermRecord where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnairePermRecord where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnairePermRecordDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnairePermRecordDTO where
  toJSON = genericToJSON jsonOptions
