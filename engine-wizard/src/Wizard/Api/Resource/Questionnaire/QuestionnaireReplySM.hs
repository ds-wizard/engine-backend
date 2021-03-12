module Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Model.Questionnaire.QuestionnaireReply

instance ToSchema Reply where
  declareNamedSchema = simpleToSchema' "_reply" (fst rQ1Updated)

instance ToSchema ReplyValue where
  declareNamedSchema = simpleToSchema' "_reply" (snd rQ1 ^. value)

instance ToSchema IntegrationReplyType where
  declareNamedSchema = simpleToSchema' "_value" rQ10IntValue
