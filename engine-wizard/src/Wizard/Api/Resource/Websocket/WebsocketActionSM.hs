module Wizard.Api.Resource.Websocket.WebsocketActionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Websocket.QuestionnaireActionSM ()
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireActions

instance ToSchema resDto => ToSchema (Success_ServerActionDTO resDto) where
  declareNamedSchema = toSwagger (Success_ServerActionDTO ensureOnlineUserAction)
