module Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM where

import Data.Swagger

import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance ToSchema ReplyValueDTO

instance ToSchema IntegrationReplyValueDTO
