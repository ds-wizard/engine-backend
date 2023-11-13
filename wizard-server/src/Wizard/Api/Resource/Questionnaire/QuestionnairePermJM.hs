module Wizard.Api.Resource.Questionnaire.QuestionnairePermJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Acl.MemberJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.QuestionnairePerm

instance FromJSON QuestionnairePermType

instance ToJSON QuestionnairePermType

instance FromJSON QuestionnairePerm where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnairePerm where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnairePermDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnairePermDTO where
  toJSON = genericToJSON jsonOptions
