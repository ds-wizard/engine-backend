module Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Acl.MemberJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Model.Questionnaire.QuestionnaireAcl

instance FromJSON QuestionnairePermRecord where
  parseJSON = simpleParseJSON "_questionnairePermRecord"

instance ToJSON QuestionnairePermRecord where
  toJSON = simpleToJSON "_questionnairePermRecord"

instance FromJSON QuestionnairePermRecordDTO where
  parseJSON = simpleParseJSON "_questionnairePermRecordDTO"

instance ToJSON QuestionnairePermRecordDTO where
  toJSON = simpleToJSON "_questionnairePermRecordDTO"
