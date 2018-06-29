module Api.Handler.PublicQuestionnaire.PublicQuestionnaireHandler where

import Control.Monad.Reader (lift)
import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO ()
import Api.Resource.Questionnaire.QuestionnaireDTO ()
import Service.PublicQuestionnaire.PublicQuestionnaireService

getQuestionnairePublicA :: Endpoint
getQuestionnairePublicA = do
  eitherDto <- lift $ getPublicQuestionnaire
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error
