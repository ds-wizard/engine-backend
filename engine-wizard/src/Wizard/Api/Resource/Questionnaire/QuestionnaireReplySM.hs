module Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema ReplyDTO where
  declareNamedSchema = simpleToSchema (toReplyDTO . head $ fReplies)

instance ToSchema ReplyValueDTO

instance ToSchema IntegrationReplyValueDTO
