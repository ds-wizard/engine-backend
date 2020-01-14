module Wizard.Api.Handler.PublicQuestionnaire.PublicQuestionnaireHandler where

import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Service.PublicQuestionnaire.PublicQuestionnaireService

getQuestionnairePublicA :: Endpoint
getQuestionnairePublicA = do
  eitherDto <- runInUnauthService $ getPublicQuestionnaire
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error
