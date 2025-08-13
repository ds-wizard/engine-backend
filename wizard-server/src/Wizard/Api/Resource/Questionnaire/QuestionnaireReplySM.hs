module Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Model.Questionnaire.QuestionnaireReply
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema Reply where
  declareNamedSchema = toSwagger (fst rQ1Updated)

instance ToSchema ReplyValue where
  declareNamedSchema = toSwagger ((snd rQ1).value)

instance ToSchema IntegrationReplyType where
  declareNamedSchema = toSwagger rQ10IntValue
